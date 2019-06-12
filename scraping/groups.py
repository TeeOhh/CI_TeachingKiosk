import pandas as pd
import string

from nltk import word_tokenize
from nltk.corpus import stopwords
stops = set(stopwords.words('english'))


def fetch_and_parse_group_data(filepath):
    # load data
    data = pd.read_csv(filepath)

    # rename columns and remove any unnecessary ones
    data.columns = [name.strip().lower().replace(' ', '_') for name in list(data)]
    data = data[['group_name', 'prof', 'topics']]

    return data


def format_group_name(name_str):
    cleaned_str = name_str.strip().title().translate(str.maketrans('', '', string.punctuation))
    tokenized_name = word_tokenize(cleaned_str)
    stops_removed = [i for i in tokenized_name if i.lower() not in stops]
    return ''.join(stops_removed)


def generate_index_terms(name_str):
    # generate list of tokens without stop words
    cleaned_str = name_str.strip().lower().translate(str.maketrans('', '', string.punctuation))
    tokenized_name = word_tokenize(cleaned_str)
    stops_removed = [i for i in tokenized_name if i.lower() not in stops]

    # 2 formats for indexedProperName
    return [
        (' '.join(stops_removed[0:-1]).strip(), stops_removed[-1]),
        (' '.join(['the'] + stops_removed), 'group')
    ]


def create_group_list(data):
    group_dict = {}

    # loop over each course in data
    for index, group in data.iterrows():
        curr_group_name = format_group_name(group['group_name'])
        curr_prof_name = group['prof']
        curr_topic_name = group['topics']

        # check if group name in dict already
        if curr_group_name in group_dict:
            group_dict[curr_group_name]['profs'].add(curr_prof_name)
            group_dict[curr_group_name]['topics'].add(curr_topic_name)
        else:
            group_dict[curr_group_name] = {
                'group_name': curr_group_name,
                'pretty_group_name': group['group_name'],
                'profs': {curr_prof_name},
                'topics': {curr_topic_name}
            }

    # output as a list of groups
    return [value for key, value in group_dict.items()]


def convert_group_to_krf(course_dict):
    group_krf = []

    # create entity
    group_name = course_dict['group_name']
    group_krf.append('(isa {} NUGroup)'.format(group_name))

    # add academic topic(s)
    for topic in course_dict['topics']:
        group_krf.append('(academicTopicOf {} {})'.format(group_name, topic))

    # add prof(s)
    for prof in course_dict['profs']:
        group_krf.append('(nuGroupMember {} {})'.format(group_name, prof))

    # add indexing
    for index_text in generate_index_terms(course_dict['pretty_group_name']):
        group_krf.append('(indexedProperName (TheList {}) {} {})'.format(index_text[0], index_text[1], group_name))

    # return combined krf string
    return '\n'.join(group_krf)


def generate_krf_list(group_list):
    return '\n\n'.join([convert_group_to_krf(group_dict) for group_dict in group_list])


def scrape_groups():
    input_filepath = '../data/nu_cs_groups.csv'
    output_dir = '../krf'

    # get groups
    group_df = fetch_and_parse_group_data(input_filepath)

    # convert group to list
    group_list = create_group_list(group_df)

    # write krf
    with open('{}/groups.krf'.format(output_dir), 'w') as f:
        f.write('(in-microtheory TeachingKioskGroupsMt)\n')
        f.write('(genlMt TeachingKioskGroupsMt TeachingKioskMt)\n\n')
        f.write(generate_krf_list(group_list))

    return group_list


if __name__ == '__main__':
    scrape_groups()
