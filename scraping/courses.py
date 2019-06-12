from faculty import scrape_faculty_data

import pandas as pd
import re
import string

from nltk import word_tokenize
from nltk.corpus import stopwords
stops = set(stopwords.words('english'))


def fetch_formatted_faculty_data():
    """
    Fetches and formats data scraped from faculty webpage.
    """
    faculty_data = scrape_faculty_data()
    for faculty in faculty_data:
        curr_faculty_name = faculty['name'].strip()
        faculty_name_split = curr_faculty_name.split(' ')

        # get first and last name
        faculty_first_name = faculty_name_split[0]

        # special case for sarah van wart
        if curr_faculty_name == 'Sarah Van Wart':
            faculty_last_name = 'Van Wart'
        else:
            faculty_last_name = faculty_name_split[-1]

        # add name to dict
        faculty['first_name'] = faculty_first_name
        faculty['last_name'] = faculty_last_name

    return faculty_data


def fetch_and_parse_course_data(filepath):
    """
    Fetch and parse course data from CSV file.
    """
    # load data
    data = pd.read_csv(filepath)

    # rename columns and remove any unnecessary ones
    data.columns = [name.strip().lower().replace(' ', '_') for name in list(data)]
    data = data[['qtr', 'course', 'title', 'instructor', 'course_topic_area', 'days', 'start', 'end', 'duration']]

    # convert quarter to full name
    data['qtr'].replace({'F': 'Fall', 'W': 'Winter', 'S': 'Spring'}, inplace=True)

    # add a year column
    data = data.assign(year=data['qtr'].apply(lambda x: '2019' if x == 'Fall' else '2020'))

    # add a parsed day column
    regex = r"([MWF]|(Tu)|(Th))"
    data['days'] = data['days'].apply(lambda x: [x[0] for x in re.findall(regex, x)])

    # parse instructors
    data['instructor'] = data['instructor'].apply(lambda x: x.split('/') if x != 'STAFF' else [])

    # replace blank course_topic_area with empty string
    data['course_topic_area'].fillna('', inplace=True)

    return data


def format_course_name(name_str):
    """
    Format course name for ontologizing.
    """
    cleaned_str = name_str.strip().title().translate(str.maketrans('', '', string.punctuation))
    tokenized_name = word_tokenize(cleaned_str)
    stops_removed = [i for i in tokenized_name if i.lower() not in stops]
    return ''.join(stops_removed)


def instructor_names(instructors, faculty_list):
    """
    Format list of instructor names for ontologizing so that we have both the pretty string and the ontology string.
    """
    pretty_instructor_names = []
    ontology_instructor_names = []

    for instructor in instructors:
        for faculty in faculty_list:
            if instructor.lower().strip() == faculty['last_name'].lower().strip():
                pretty_instructor_names.append(faculty['name'])
                ontology_instructor_names.append(faculty['ID'])
                break

    return pretty_instructor_names, ontology_instructor_names


def create_course_list(data, faculty_list):
    """
    Create a course dict for each course in the scraped CSV data.
    """
    course_list = []

    # loop over each course in data
    for index, course in data.iterrows():
        formatted_names, ontology_names = instructor_names(course['instructor'], faculty_list)

        curr_course_dict = {
            'pretty_name': course['title'],
            'formatted_name': format_course_name(course['title']),
            'course_number': course['course'],
            'quarter_offered': course['qtr'],
            'year_offered': course['year'],
            'formatted_instructors': formatted_names,
            'ontology_instructors': ontology_names,
            'course_topic_area': course['course_topic_area'],
            'class_days': course['days'],
            'start_time': course['start'],
            'end_time': course['end'],
            'class_duration': course['duration']
        }
        course_list.append(curr_course_dict)

    return course_list


def day_abbrev_to_str(day_abbrev):
    """
    Convert day abbreviation into a full day string.
    """
    cleaned_day_abbrev = day_abbrev.strip().lower()

    if cleaned_day_abbrev == 'm':
        return 'Monday'
    if cleaned_day_abbrev == 'tu':
        return 'Tuesday'
    if cleaned_day_abbrev == 'w':
        return 'Wednesday'
    if cleaned_day_abbrev == 'th':
        return 'Thursday'
    if cleaned_day_abbrev == 'f':
        return 'Friday'

    return ''


def generate_time_string(days_offered, start_time, end_time):
    """
    Generate a course time string in  the format 'Monday/Wednesday from 14:00 to 14:50'
    :return:
    """
    # convert day abbreviation to full text string
    day_strs = [day_abbrev_to_str(day) for day in days_offered]

    # create and return string
    return '{} from {} to {}'.format('/'.join(day_strs), start_time, end_time)


def convert_course_to_krf(course_dict):
    """
    Generate KRF for single course.
    """
    course_krf = []

    # setup course name
    course_name = '{}-{}{}'.format(course_dict['formatted_name'],
                                   course_dict['quarter_offered'],
                                   course_dict['year_offered'])
    course_krf.append('(isa {} NUCourse-CS)'.format(course_name))
    course_krf.append('(courseName {} "{}")'.format(course_name, course_dict['pretty_name']))

    # setup course field
    if course_dict['course_topic_area'] != '':
        course_krf.append('(academicTopicOf {} {})'.format(course_name, course_dict['course_topic_area']))

    # setup course time
    course_time_str = generate_time_string(course_dict['class_days'],
                                           course_dict['start_time'],
                                           course_dict['end_time'])
    course_krf.append('(courseTimeString {} "{}")'.format(course_name, course_time_str))

    # setup course instructor(s)
    for instructor in course_dict['ontology_instructors']:
        course_krf.append('(courseInstructor {} {})'.format(course_name, instructor))

    # setup index predicates for EA NLU
    course_krf.append('(indexedProperName (TheList) {} {})'.format(course_dict['course_number'], course_name))
    course_krf.append('(indexedProperName (TheList cs) {} {})'.format(course_dict['course_number'], course_name))
    course_krf.append('(indexedProperName (TheList eecs) {} {})'.format(course_dict['course_number'], course_name))

    # return combined krf string
    return '\n'.join(course_krf)


def generate_krf_list(course_list):
    """
    Generate KRF for all courses.
    """
    return '\n\n'.join([convert_course_to_krf(course_dict) for course_dict in course_list])


def scrape_courses():
    """
    Run scraper
    """
    input_filepath = '../data/cs-courses_2019-2020.csv'
    output_dir = '../krf'

    # get faculty list and courses
    faculty_list = fetch_formatted_faculty_data()
    course_df = fetch_and_parse_course_data(input_filepath)

    # create course lists for each quarter separately and export
    qtr_course_info_list = [
        {'quarter': 'Fall', 'outfile': 'F2019'},
        {'quarter': 'Winter', 'outfile': 'W2019'},
        {'quarter': 'Spring', 'outfile': 'S2019'}
    ]

    for qtr in qtr_course_info_list:
        curr_qtr_str = qtr['quarter']
        curr_outfile = qtr['outfile']
        curr_course_list = create_course_list(course_df[course_df['qtr'] == curr_qtr_str], faculty_list)

        mt_str = ''
        if qtr['quarter'] == 'Fall':
            mt_str = '(in-microtheory TeachingKioskCoursesFall2019Mt)'
        elif qtr['quarter'] == 'Winter':
            mt_str = '(in-microtheory TeachingKioskCoursesWinter2019Mt)'
        elif qtr['quarter'] == 'Spring':
            mt_str = '(in-microtheory TeachingKioskCoursesSpring2019Mt)'

        with open('{}/courses-{}.krf'.format(output_dir, curr_outfile), 'w') as f:
            f.write('{}\n\n'.format(mt_str))
            f.write(generate_krf_list(curr_course_list))

    # also export full course list
    full_course_list = create_course_list(course_df, faculty_list)
    with open('{}/courses-2019-2020.krf'.format(output_dir), 'w') as f:
        f.write('(in-microtheory TeachingKioskCourses2019Mt)\n')
        f.write('(genlMt TeachingKioskCourses2019Mt TeachingKioskMt)\n\n')
        f.write(generate_krf_list(full_course_list))

    return create_course_list(course_df, faculty_list)


if __name__ == '__main__':
    scrape_courses()
