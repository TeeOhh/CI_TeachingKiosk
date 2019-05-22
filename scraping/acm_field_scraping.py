# data loading and manipulation
import copy
import os
import requests
import json

import bs4 as bs

import string
import re
from nltk.corpus import stopwords
stops = set(stopwords.words("english"))

import urllib.request
import ssl
import html5lib
context = ssl._create_unverified_context()

# from https://stackoverflow.com/questions/17850121/parsing-nested-html-list-with-beautifulsoup
def dictify(ul, parent):
    result = []
    for li in ul.find_all('li', recursive=False):
        key = next(li.stripped_strings)
        ul = li.find('ul')
        if ul:
            result.append({
                'name': key,
                'children': dictify(ul, key)
            })
    return result

def max_depth(tree, curr_depth):
    # base case: no more children
    if len(tree) == 0:
        return curr_depth

    # recurse deeper
    curr_max_depth = curr_depth
    for child in tree:
        new_max_depth = max_depth(child['children'], curr_depth + 1)
        if new_max_depth > curr_max_depth:
            curr_max_depth = new_max_depth

    return curr_max_depth


def print_nodes_at_depth(tree, parent, curr_depth, desired_depth):
    """
    Gets nodes at the desired depth level.
    """
    # base case: no more children or depth limit reached
    if len(tree) == 0 or curr_depth > desired_depth:
        return

    # recurse deeper
    if curr_depth == desired_depth:
        print('Nodes for {}'.format(parent))
    for child in tree:
        if curr_depth == desired_depth:
            print(child['name'])
        print_nodes_at_depth(child['children'], child['name'], curr_depth + 1, desired_depth)

    print()
    return


def get_nodes_at_depth(tree, parent, curr_depth, desired_depth):
    """
    Gets nodes at the desired depth level.
    """
    # base case: no more children or depth limit reached
    if len(tree) == 0 or curr_depth > desired_depth:
        return

    # recurse deeper and remove all children if at the desired depth
    for child in tree:
        if curr_depth == desired_depth:
            child['children'] = []
        else:
            get_nodes_at_depth(child['children'], child['name'], curr_depth + 1, desired_depth)

    return tree

def index_of_key(list_of_dicts, field, target):
    for index, curr_dict in enumerate(list_of_dicts):
        if curr_dict[field] == target:
            return index

def add_parent(tree, curr_parent=None):
    # base case: no more children
    if len(tree) == 0:
        return

    for child in tree:
        # set current parent
        child['parent'] = curr_parent

        # recurse deeper
        new_max_depth = add_parent(child['children'], curr_parent=child['name'])

    return

def clean_topic_name(topic_name):
    # remove any text between parens
    cleaned_topic_name = re.sub("([\(\[]).*?([\)\]])", "\g<1>\g<2>", topic_name)

    # remove any puncutation and convert to lowercase
    cleaned_topic_name = cleaned_topic_name.translate(str.maketrans(string.punctuation, ' ' * len(string.punctuation))).lower()

    # remove stop words and make CamelCase
    cleaned_topic_name = ''.join([word.title() for word in cleaned_topic_name.split() if not word.isspace() and word not in stops])

    # append -Topic to each topic name
    cleaned_topic_name += '-Topic'

    return cleaned_topic_name

def generate_krf_as_list(tree, krf_list):
    # base case: no more children
    if len(tree) == 0:
        return krf_list

    # recurse deeper
    for child in tree:
        # SPECIAL CASE: CS should be added as an AcademicTopic but without -Topic
        if child['name'] != 'ComputerScience':
            curr_child_name = clean_topic_name(child['name'])
            krf_list.append('(isa {} AcademicTopic)'.format(curr_child_name))

            if child['parent'] is not None:
                # SPECIAL CASE: ignore CS parent when cleaning topic name
                curr_parent_name = ''
                if child['parent'] == 'ComputerScience':
                    curr_parent_name = child['parent']
                else:
                    curr_parent_name = clean_topic_name(child['parent'])

                krf_list.append('(directSubTopic {} {})'.format(curr_parent_name, curr_child_name))
        else:
            krf_list.append('(isa {} AcademicTopic)'.format(child['name']))

        krf_list = generate_krf_as_list(child['children'], krf_list)

    return krf_list

def main():
  # check if source is already downloaded
  css_source_file = './acm_ccs_source.txt'
  ccs_source = ''

  if os.path.isfile(css_source_file):
      with open(css_source_file, 'r') as f:
          ccs_source = f.read();
  else:
      acm_ccs_url = 'https://dl.acm.org/ccs/ccs_flat.cfm#10003120'
      ccs_source = urllib.request.urlopen(acm_ccs_url, context=context).read()

  ccs_parsed = bs.BeautifulSoup(ccs_source, 'html5lib')

  # generate output from html parse tree
  flat_content = ccs_parsed.find('div', id='holdflat').ul
  output = dictify(flat_content, None)

  # remove general and reference
  output = [x for x in output if x['name'] != 'General and reference']

  # add all subfields of HCI into HCI, and replace hcc with hci
  hcc_index = index_of_key(output, 'name', 'Human-centered computing')
  hci_index = index_of_key(output[hcc_index]['children'], 'name', 'Human computer interaction (HCI)')

  new_hci_children = [child for index, child in enumerate(output[hcc_index]['children']) if index != hci_index]
  output[hcc_index]['children'][hci_index]['children'] += new_hci_children

  # remove human-centered computing and replace with hci
  output[hcc_index] = output[hcc_index]['children'][hci_index]

  # combine stuff under AI and make its own top-level grouping
  comp_method_index = index_of_key(output, 'name', 'Computing methodologies')
  ai_index = index_of_key(output[comp_method_index]['children'], 'name', 'Artificial intelligence')
  symb_index = index_of_key(output[comp_method_index]['children'], 'name', 'Symbolic and algebraic manipulation')
  ml_index = index_of_key(output[comp_method_index]['children'], 'name', 'Machine learning')
  modeling_index = index_of_key(output[comp_method_index]['children'], 'name', 'Modeling and simulation')

  output[comp_method_index]['children'][ai_index]['children'] += [output[comp_method_index]['children'][symb_index],
                                                                  output[comp_method_index]['children'][ml_index],
                                                                  output[comp_method_index]['children'][modeling_index]]
  output += [output[comp_method_index]['children'][ai_index]]

  # add others from Computing methodologies
  output += [child for index, child in enumerate(output[comp_method_index]['children'])
             if index not in set([ai_index, symb_index, ml_index, modeling_index])]

  # delete Computing methodologies
  del output[comp_method_index]

  # put everything under CS
  output = [{
      'name': 'ComputerScience',
      'children': output,
      'parent': None
  }]

  # add parents
  add_parent(output)

  return output

if __name__ == '__main__':
  # scrape ACM CCS
  output = main()

  # save scraped tree as json
  with open('./acm_scraped_fields.json', 'w') as outfile:
    json.dump(output, outfile)

  # save small scraped tree as json
  with open('./acm_scraped_fields_small.json', 'w') as outfile:
    json.dump(get_nodes_at_depth(copy.deepcopy(output), None, 1, 4), outfile)

  # generate krf from output
  krf_list = generate_krf_as_list(output, [])
  with open('../krf/academic-fields.krf', 'w') as f:
      f.write('(in-microtheory TeachingKioskMt)\n\n')
      f.write('\n'.join(krf_list))

  # generate small krf with depth limit  = 4
  krf_list_small = generate_krf_as_list(get_nodes_at_depth(copy.deepcopy(output), None, 1, 4), [])
  with open('../krf/academic-fields-small.krf', 'w') as f:
      f.write('(in-microtheory TeachingKioskMt)\n\n')
      f.write('\n'.join(krf_list))