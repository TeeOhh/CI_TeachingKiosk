import copy
import os
import json

import urllib.request
import ssl

import bs4 as bs

import string
import re

from nltk import word_tokenize
from nltk.corpus import stopwords
stops = set(stopwords.words('english'))

# set ssl context
context = ssl._create_unverified_context()


# from https://stackoverflow.com/questions/17850121/parsing-nested-html-list-with-beautifulsoup
def dictify(ul):
    result = []
    for li in ul.find_all('li', recursive=False):
        key = next(li.stripped_strings)
        ul = li.find('ul')
        if ul:
            result.append({
                'name': key,
                'children': dictify(ul)
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


def get_nodes_at_depth(tree, curr_depth, desired_depth):
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
            get_nodes_at_depth(child['children'], curr_depth + 1, desired_depth)

    return tree


def key_index(list_of_dicts, field, target):
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
        add_parent(child['children'], curr_parent=child['name'])

    return


def clean_topic_name(topic_name):
    # remove any text between parens
    cleaned_topic_name = re.sub("([(\[]).*?([)\]])", "\g<1>\g<2>", topic_name)

    # remove any punctuation and convert to lowercase
    cleaned_topic_name = cleaned_topic_name.translate(str.maketrans(string.punctuation, ' ' * len(string.punctuation)))
    cleaned_topic_name = cleaned_topic_name.lower()

    # remove stop words and make CamelCase
    cleaned_topic_name = ''.join([word.title() for word in cleaned_topic_name.split() if not word.isspace()
                                  and word not in stops])

    # append -Topic to each topic name
    cleaned_topic_name += '-Topic'

    return cleaned_topic_name


def generate_index_terms(name_str):
    # generate list of tokens without stop words
    cleaned_str = name_str.strip().lower().translate(str.maketrans('', '', string.punctuation))
    tokenized_name = word_tokenize(cleaned_str)
    stops_removed = [i for i in tokenized_name if i.lower() not in stops]

    # SPECIAL CASE FOR GRAPHICS
    for index, word in enumerate(stops_removed):
        if word == 'graphics':
            stops_removed[index] = 'graphicss'

    # 3 formats for indexedProperName
    return [
        (' '.join(stops_removed[0:-1]).strip(), stops_removed[-1]),
        (' '.join(['the'] + stops_removed), 'topic'),
        (' '.join(['the'] + stops_removed), 'subtopic')
    ]


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

                krf_list.append('(subTopicOf {} {} 1)'.format(curr_parent_name, curr_child_name))

            # add indexedProperName
            for index_text in generate_index_terms(child['name']):
                krf_list.append('(indexedProperName (TheList {}) {} {})'.format(index_text[0],
                                                                                index_text[1],
                                                                                curr_child_name))

        else:
            krf_list.append('(isa {} AcademicTopic)'.format(child['name']))
            krf_list.append('(indexedProperName (TheList computer) science ComputerScience)')
            krf_list.append('(indexedProperName (TheList the computer science) field ComputerScience)')
            krf_list.append('(indexedProperName (TheList the computer science) topic ComputerScience)')
            krf_list.append('(indexedProperName (TheList ) cs ComputerScience)')
            krf_list.append('(indexedProperName (TheList ) eecs ComputerScience)')

        # get krf for children
        krf_list = generate_krf_as_list(child['children'], krf_list)

    return krf_list


def scrape_and_parse_page():
    # check if source is already downloaded
    css_source_file = '../data/acm_ccs_source.txt'
    ccs_source = ''

    if os.path.isfile(css_source_file):
        with open(css_source_file, 'r') as f:
            ccs_source = f.read()
    else:
        acm_ccs_url = 'https://dl.acm.org/ccs/ccs_flat.cfm#10003120'
        ccs_source = urllib.request.urlopen(acm_ccs_url, context=context).read()

    ccs_parsed = bs.BeautifulSoup(ccs_source, 'html5lib')

    # generate output from html parse tree
    flat_content = ccs_parsed.find('div', id='holdflat').ul
    parsed_content = dictify(flat_content)

    # remove general and reference
    parsed_content = [x for x in parsed_content if x['name'] != 'General and reference']

    # add all subfields of HCI into HCI, and replace hcc with hci
    hcc_index = key_index(parsed_content, 'name', 'Human-centered computing')
    hci_index = key_index(parsed_content[hcc_index]['children'], 'name', 'Human computer interaction (HCI)')

    new_hci_children = [child for i, child in enumerate(parsed_content[hcc_index]['children']) if i != hci_index]
    parsed_content[hcc_index]['children'][hci_index]['children'] += new_hci_children

    # remove human-centered computing and replace with hci
    parsed_content[hcc_index] = parsed_content[hcc_index]['children'][hci_index]

    # combine stuff under AI and make its own top-level grouping
    comp_method_index = key_index(parsed_content, 'name', 'Computing methodologies')
    ai_index = key_index(parsed_content[comp_method_index]['children'], 'name', 'Artificial intelligence')
    symb_index = key_index(parsed_content[comp_method_index]['children'], 'name', 'Symbolic and algebraic manipulation')
    ml_index = key_index(parsed_content[comp_method_index]['children'], 'name', 'Machine learning')
    modeling_index = key_index(parsed_content[comp_method_index]['children'], 'name', 'Modeling and simulation')

    new_ai_children = [parsed_content[comp_method_index]['children'][symb_index],
                       parsed_content[comp_method_index]['children'][ml_index],
                       parsed_content[comp_method_index]['children'][modeling_index]]
    parsed_content[comp_method_index]['children'][ai_index]['children'] += new_ai_children
    parsed_content += [parsed_content[comp_method_index]['children'][ai_index]]

    # add others from Computing methodologies
    parsed_content += [child for index, child in enumerate(parsed_content[comp_method_index]['children'])
                       if index not in {ai_index, symb_index, ml_index, modeling_index}]

    # delete Computing methodologies
    del parsed_content[comp_method_index]

    # put everything under CS
    parsed_content = [{
        'name': 'ComputerScience',
        'children': parsed_content,
        'parent': None
    }]

    # add parents
    add_parent(parsed_content)

    return parsed_content


def scrape_cs_fields():
    # scrape ACM CCS
    scraped_data = scrape_and_parse_page()

    # save scraped tree as json
    with open('../json/acm-scraped-fields.json', 'w') as outfile:
        json.dump(scraped_data, outfile, indent=4)

    # save small scraped tree as json
    with open('../json/acm-scraped-fields-small.json', 'w') as outfile:
        json.dump(get_nodes_at_depth(copy.deepcopy(scraped_data), 1, 4), outfile, indent=4)

    # generate full krf from scraped_data
    with open('../krf/academic-fields.krf', 'w') as outfile:
        outfile.write('(in-microtheory TeachingKioskMt)\n\n')
        outfile.write('\n'.join(generate_krf_as_list(scraped_data, [])))

    # generate small krf with depth limit = 3
    level = 3
    with open('../krf/academic-fields-small-level{}.krf'.format(level), 'w') as outfile:
        outfile.write('(in-microtheory TeachingKioskMt)\n\n')
        outfile.write('\n'.join(generate_krf_as_list(get_nodes_at_depth(copy.deepcopy(scraped_data), 1, level), [])))

    # generate small krf with depth limit  = 4
    level = 4
    with open('../krf/academic-fields-small-level{}.krf'.format(level), 'w') as outfile:
        outfile.write('(in-microtheory TeachingKioskMt)\n\n')
        outfile.write('\n'.join(generate_krf_as_list(get_nodes_at_depth(copy.deepcopy(scraped_data), 1, level), [])))


if __name__ == '__main__':
    scrape_cs_fields()