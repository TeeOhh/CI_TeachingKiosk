import requests
from lxml import html

from colleges import fetch_colleges
from helpers import *


def scrape_faculty_data():
    # IDENTIFIERS FOR NAVIGATING FACULTY PAGE:
    base_page = 'http://www.mccormick.northwestern.edu'
    eecs_people = '/computer-science/people/'
    faculty_page = base_page + eecs_people

    # list of faculty pages, href to each faculty details page
    faculty_pages = '//div[@class="faculty-info"]/h3/a[@href]/@href'

    # attributes to look for on faculty details page
    faculty_name = '//h1[@id="page-title"]/text()'
    address = '//div[@id="faculty-profile-left"]/text()'
    phone_link = '//a[@class="phone_link"]/@href'
    mail_link = '//a[@class="mail_link"]/@href'
    webpages = ('//p[following-sibling::h2 = "Departments" and preceding-'
                'sibling::h2[@class = "sites-header"] = "Website"]'
                '/a[@href]/@href')
    webpagenames = ('//p[following-sibling::h2 = "Departments" and preceding-'
                    'sibling::h2[@class = "sites-header"] = "Website"]/'
                    'a[@href]/text()')
    departments = ('//p[following-sibling::h2 = "Affiliations" and preceding'
                   '-sibling::h2 = "Departments"]/a[@href]/text()')
    affiliations = ('//p[preceding-sibling::h2 = "Affiliations"]/a[@href and '
                    'not(text() = "Download CV")]/text()')
    cv_href = ('//p[preceding-sibling::h2 = "Affiliations"]/a[@href and '
               'text() = "Download CV"]/@href')
    education = ('//p[preceding-sibling::h2 = "Education" and following-'
                 'sibling::h2 = "Research Interests"]/text()')
    interests = '//p[preceding-sibling::h2 = "Research Interests"]//text()'

    # SCRAPE:
    # get base cs faculty listing website
    base_content = requests.get(faculty_page).content
    base_tree = html.fromstring(base_content)

    # grab faculty from page as list,
    faculty_pages = base_tree.xpath(faculty_pages)

    # loop over every faculty on this page and collect scraped faculty data
    all_faculty = []
    for faculty_page in faculty_pages:
        # setup faculty data for each faculty
        faculty = dict.fromkeys(['name', 'ID', 'address', 'cv_link',
                                 'phone_number', 'personal_site', 'email',
                                 'departments', 'affiliations', 'websites',
                                 'education', 'interests'], None)

        # go to specific faculty detail page, check that page is one of ours
        # (3 pages are not and have hull https:// links...)
        if not faculty_page.startswith('http'):
            faculty_page = 'http:' + faculty_page

        content = requests.get(faculty_page).content
        tree = html.fromstring(content)

        # scrape faculty data
        faculty['name'] = xpath_to_clean_string(tree, faculty_name)
        faculty['ID'] = unique_id(faculty['name'])
        faculty['address'] = xpath_address(tree, address)
        faculty['room_number'] = xpath_room_number(tree, address)
        faculty['phone_number'] = xpath_phone_number(tree, phone_link)
        faculty['email'] = xpath_email(tree, mail_link)
        faculty['websites'] = xpath_zip_lists(tree, webpages, webpagenames)
        faculty['personal_site'] = personal_site(faculty)
        faculty['departments'] = xpath_to_list(tree, departments)
        faculty['affiliations'] = xpath_to_list(tree, affiliations)
        faculty['cv_link'] = xpath_to_clean_string(tree, cv_href)
        faculty['education'] = xpath_to_list(tree, education)
        faculty['interests'] = xpath_to_clean_string(tree, interests)

        # collect all faculty data
        all_faculty.append(faculty)
    return all_faculty


def ontologize_faculty_data(faculty, colleges):
    args = '(SocialModelMtFn ' + faculty['ID'] + ')'
    meld = lisp_style('in-microtheory', [args]) + '\n'
    meld += isa(faculty['ID'], 'NUPerson') + '\n'
    meld += isa(faculty['ID'], 'NUFaculty') + '\n'
    meld += lisp_style('department', [faculty['ID'], 'ComputerScience']) + '\n'

    if faculty['departments']:
        for department in faculty['departments']:
            if department != 'Electrical Engineering and Computer Science':
                args = [faculty['ID'], unique_id(department)]
                meld += lisp_style('department', args) + '\n'

    if faculty['education'] is not None:
        # degreeInField
        for edu in faculty['education']:
            if 'Ph.D' in edu:
                meld += isa(faculty['ID'], 'AcademicProfessional') + '\n'
                args = [faculty['ID'], 'Dr']
                meld += lisp_style('titleOfPerson', args) + '\n'
                break

        for edu in faculty['education']:
            all_degrees = [{'degree': 'Ph.D', 'normal': 'PhD'},
                           {'degree': 'M.S', 'normal': 'MS'},
                           {'degree': 'MS', 'normal': 'MS'},
                           {'degree': 'M.A', 'normal': 'MA'},
                           {'degree': 'B.S', 'normal': 'BS'},
                           {'degree': 'BS', 'normal': 'BS'},
                           {'degree': 'B.A', 'normal': 'BA'},
                           {'degree': 'B.E', 'normal': 'BE'},
                           {'degree': 'M.E', 'normal': 'ME'},
                           {'degree': 'M. Mus', 'normal': 'MM'},
                           {'degree': 'B. Mus', 'normal': 'BM'},
                           {'degree': 'Master of Fine Arts', 'normal': 'MFA'},
                           {'degree': 'B. Tech', 'normal': 'BT'},
                           {'degree': 'A.B', 'normal': 'AB'},
                           {'degree': 'S.M', 'normal': 'SM'},
                           {'degree': 'S.B', 'normal': 'SB'},
                           {'degree': 'B.CSci', 'normal': 'BCS'}]

            for degree in all_degrees:
                if degree['degree'] in edu:
                    faculty_id = faculty['ID']
                    meld += schooling(edu, degree['normal'], colleges, faculty_id)
                    break

    if faculty['phone_number']:
        phone = faculty['phone_number']
        meld += lisp_style('phoneNumberOf', [faculty['ID'], '"' + phone + '"'])
        meld += '\n'

    args = [faculty['ID'], '"' + faculty['email'] + '"']
    meld += lisp_style('emailOf', args) + '\n'

    if faculty['personal_site']:
        args = [faculty['ID'], '"' + faculty['personal_site'] + '"']
        meld += lisp_style('personalWebsite', args) + '\n'

    args = [faculty['ID'], '"' + faculty['room_number'] + '"']
    meld += lisp_style('officeLocation', args) + '\n'
    meld += lisp_style('in-microtheory', ['EnglishMt']) + '\n'
    meld += lisp_style('fullName', [faculty['ID'], '"' + faculty['name'] + '"'])

    split_name = faculty['name'].split(' ')
    last_name = split_name[-1:][0]
    args = ['(TheList professor)', last_name, faculty['ID']]
    meld += '\n' + lisp_style('indexedProperName', args) + '\n'
    args = ['(TheList ' + split_name[0] + ')', last_name, faculty['ID']]
    meld += lisp_style('indexedProperName', args) + '\n'
    args = ['(TheList doctor)', last_name, faculty['ID']]
    meld += lisp_style('indexedProperName', args) + '\n'

    return meld + '\n'


def xpath_zip_lists(source, xpath_text_1, xpath_text_2):
    xpath_list_1 = xpath_to_list(source, xpath_text_1)
    xpath_list_2 = xpath_to_list(source, xpath_text_2)
    if xpath_list_1 and xpath_list_2:
        return list(zip(xpath_list_1, xpath_list_2))
    return []


def xpath_address(source, xpath_text):
    xpath_list = xpath_to_list(source, xpath_text)
    spacing = xpath_list[0] + ' ' + xpath_list[2]
    return spacing if len(xpath_list) > 0 else None


def xpath_room_number(source, xpath_text):
    xpath_list = xpath_to_list(source, xpath_text)
    if len(xpath_list) > 1:
        return xpath_list[1]
    return None


def xpath_phone_number(source, xpath_text):
    phone_number = xpath_to_clean_string(source, xpath_text)
    if phone_number:
        return phone_number[6:]  # remove the phone number link tel://
    return None


def xpath_email(source, xpath_text):
    email = xpath_to_clean_string(source, xpath_text)
    if email:
        return email[7:]  # remove the email link mailto:
    return None


def personal_site(faculty_data):
    if len(faculty_data['websites']) > 0:
        for page, pageName in faculty_data['websites']:
            if faculty_data['name'] in pageName:
                return page
    return None


def schooling(edu, degree, colleges, faculty_id):
    meld = ''
    splitedu = edu.split(',')
    found = False
    options = []

    for college in colleges:
        if college in edu:
            args = [faculty_id, unique_id(college), degree]
            meld += lisp_style('schooling', args) + '\n'
            found = True
            break  # Exact match, look no further

        if len(splitedu) > 1:
            if splitedu[1] in college:
                options.append(college)
    if not found:
        if len(options) > 0:
            college_id = unique_id(options[0])
            args = [faculty_id, college_id, degree]
            meld += lisp_style('schooling', args) + '\n'
        elif len(options) > 1:
            for option in options:
                if splitedu[2] in option:
                    college_id = unique_id(option)
                    args = [faculty_id, college_id, degree]
                    meld += lisp_style('schooling', args) + '\n'
                    break

        extras = [{'text': 'MIT',
                   'id': 'MassachusettsInstituteofTechnology'},
                  {'text': 'M.I.T.',
                   'id': 'MassachusettsInstituteofTechnology'},
                  {'text': 'UCLA',
                   'id': 'UniversityofCaliforniaLosAngeles'},
                  {'text': 'State University of New York, Stony Brook',
                   'id': 'StonyBrookUniversity'},  # SUNY schools are confusing
                  {'text': 'University of Illinois at Urbana',
                   'id': 'UniversityofIllinoisatUrbanaChampaign'},
                  {'text': 'UC Santa Barbara',
                   'id': 'UniversityofCaliforniaSantaBarbara'},
                  {'text': 'Aristotelian University of Thessaloniki',
                   'id': 'AristotleUniversityofThessaloniki'},
                  {'text': 'Bogazici Univeristy',
                   'id': 'BogaziciUniveristy'},  # BogaziçiÜniversitesi
                  {'text': 'Tel-Aviv University',
                   'id': 'TelAvivUniversity'},
                  {'text': 'University of Belgrade',
                   'id': 'UniversityofBelgrade'},  # not listed at all...
                  {'text': 'Brown Univesity',
                   'id': 'BrownUniversity'},
                  {'text': 'University of Sts. Kiril and Metodij',
                   'id': 'SaintsCyrilandMethodiusUniversityofSkopje'},  # not listed at all...
                  {'text': 'Indian Institute of Technology, Madras',
                   'id': 'IndianInstituteofTechnologyMadras'},
                  {'text': 'I am an Associate Professor at the EECS',
                   'id': 'Princeton University'}]

        for extra in extras:
            if extra['text'] in edu:
                args = [faculty_id, extra['id'], degree]
                meld += lisp_style('schooling', args) + '\n'
                break

    return meld


def generate_krf_for_faculty(colleges):
    meld = ''
    for faculty in scrape_faculty_data():
        meld += ontologize_faculty_data(faculty, colleges)
    return meld.strip()


def scrape_faculty():
    faculty_meld = generate_krf_for_faculty(fetch_colleges())
    to_file(faculty_meld, '../krf', 'faculty')


if __name__ == '__main__':
    scrape_faculty()
