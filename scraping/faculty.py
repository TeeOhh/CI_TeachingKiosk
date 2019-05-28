from datetime import datetime, timedelta, date
from selenium import webdriver
from lxml import html
import requests
import regex as re
import string
import os
import time

from colleges import scrapeColleges
from helpers import *

def scrapeFacultyData():
  # IDENTIFIERS FOR NAVIGATING FACULTY PAGE:
  BASE_PAGE = 'http://www.mccormick.northwestern.edu'
  EECS_PEOPLE = '/computer-science/people/'
  FACULTY_PAGE = BASE_PAGE + EECS_PEOPLE
  # list of faculty pages, href to each faculty details page
  FACULTY_PAGES = '//div[@class="faculty-info"]/h3/a[@href]/@href'
  # attributes to look for on faculty details page
  FACULTY_NAME = '//h1[@id="page-title"]/text()'
  ADDRESS      = '//div[@id="faculty-profile-left"]/text()'
  PHONE_LINK   = '//a[@class="phone_link"]/@href'
  MAIL_LINK    = '//a[@class="mail_link"]/@href'
  WEBPAGES     = ('//p[following-sibling::h2 = "Departments" and preceding-'
          'sibling::h2[@class = "sites-header"] = "Website"]'
          '/a[@href]/@href')
  WEBPAGENAMES = ('//p[following-sibling::h2 = "Departments" and preceding-'
          'sibling::h2[@class = "sites-header"] = "Website"]/'
          'a[@href]/text()')
  DEPARTMENTS  = ('//p[following-sibling::h2 = "Affiliations" and preceding'
            '-sibling::h2 = "Departments"]/a[@href]/text()')
  AFFILIATIONS = ('//p[preceding-sibling::h2 = "Affiliations"]/a[@href and '
          'not(text() = "Download CV")]/text()')
  CV_HREF      = ('//p[preceding-sibling::h2 = "Affiliations"]/a[@href and '
            'text() = "Download CV"]/@href')
  EDUCATION    = ('//p[preceding-sibling::h2 = "Education" and following-'
          'sibling::h2 = "Research Interests"]/text()')
  INTERESTS    = '//p[preceding-sibling::h2 = "Research Interests"]//text()'
  # SCRAPE:
  # get base cs faculty listing website
  base_content = requests.get(FACULTY_PAGE).content
  base_tree = html.fromstring(base_content)

  # grab faculty from page as list,
  faculty_pages = base_tree.xpath(FACULTY_PAGES)

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
    faculty['name']          = xpathToCleanString(tree, FACULTY_NAME)
    faculty['ID']            = uniqueID(faculty['name'])
    faculty['address']       = xpathAddress(tree, ADDRESS)
    faculty['room_number']   = xpathRoomNumber(tree, ADDRESS)
    faculty['phone_number']  = xpathPhoneNumber(tree, PHONE_LINK)
    faculty['email']         = xpathEmail(tree, MAIL_LINK)
    faculty['websites']      = xpathZipLists(tree, WEBPAGES, WEBPAGENAMES)
    faculty['personal_site'] = personalSite(faculty)
    faculty['departments']   = xpathToList(tree, DEPARTMENTS)
    faculty['affiliations']  = xpathToList(tree, AFFILIATIONS)
    faculty['cv_link']       = xpathToCleanString(tree, CV_HREF)
    faculty['education']     = xpathToList(tree, EDUCATION)
    faculty['interests']     = xpathToCleanString(tree, INTERESTS)
    # collect all faculty data
    all_faculty.append(faculty)
  return all_faculty


def getFacultyOntologized(faculty, colleges):
  args = '(SocialModelMtFn ' + faculty['ID'] + ')'
  meld = lispStyle('in-microtheory', [args]) + '\n'
  meld += isa(faculty['ID'], 'NUPerson') + '\n'
  meld += isa(faculty['ID'], 'NUFaculty') + '\n'
  meld += lispStyle('department', [faculty['ID'], 'ComputerScience']) + '\n'
  if faculty['departments']:
    for department in faculty['departments']:
      if department != 'Electrical Engineering and Computer Science':
        args = [faculty['ID'], uniqueID(department)]
        meld += lispStyle('department', args) + '\n'
  if faculty['education'] is not None:
    # degreeInField
    for edu in faculty['education']:
      if 'Ph.D' in edu:
        meld += isa(faculty['ID'], 'AcademicProfessional') + '\n'
        args = [faculty['ID'], 'Dr']
        meld += lispStyle('titleOfPerson', args) + '\n'
        break
    for edu in faculty['education']:
      all_degrees = [{'degree':'Ph.D',    'normal':'PhD'},
               {'degree':'M.S',     'normal':'MS'},
               {'degree':'MS',      'normal':'MS'},
               {'degree':'M.A',     'normal':'MA'},
               {'degree':'B.S',     'normal':'BS'},
               {'degree':'BS',      'normal':'BS'},
               {'degree':'B.A',     'normal':'BA'},
               {'degree':'B.E',     'normal':'BE'},
               {'degree':'M.E',     'normal':'ME'},
               {'degree':'M. Mus',  'normal':'MM'},
               {'degree':'B. Mus',  'normal':'BM'},
               {'degree':'Master of Fine Arts', 'normal':'MFA'},
               {'degree':'B. Tech', 'normal':'BT'},
               {'degree':'A.B',     'normal':'AB'},
               {'degree':'S.M',     'normal':'SM'},
               {'degree':'S.B',     'normal':'SB'},
               {'degree':'B.CSci',  'normal':'BCS'}]
      for degree in all_degrees:
        if degree['degree'] in edu:
          ID = faculty['ID']
          meld += schooling(edu, degree['normal'], colleges, ID)
          break
  if faculty['phone_number']:
    phone = faculty['phone_number']
    meld += lispStyle('phoneNumberOf', [faculty['ID'], '"' + phone + '"'])
    meld += '\n'
  args = [faculty['ID'], '"' + faculty['email'] + '"']
  meld += lispStyle('emailOf', args) + '\n'
  if faculty['personal_site']:
    args = [faculty['ID'], '"' + faculty['personal_site'] + '"']
    meld += lispStyle('personalWebsite', args) + '\n'
  args = [faculty['ID'], '"' + faculty['room_number'] + '"']
  meld += lispStyle('officeLocation', args) + '\n'
  meld += lispStyle('in-microtheory', ['EnglishMt']) + '\n'
  meld += lispStyle('fullName', [faculty['ID'], '"' + faculty['name'] + '"'])
  splitName = faculty['name'].split(' ')
  lastName = splitName[-1:][0]
  args = ['(TheList professor)', lastName, faculty['ID']]
  meld += '\n' + lispStyle('indexedProperName', args) + '\n'
  args = ['(TheList ' + splitName[0] + ')', lastName, faculty['ID']]
  meld += lispStyle('indexedProperName', args) + '\n'
  args = ['(TheList doctor)', lastName, faculty['ID']]
  meld += lispStyle('indexedProperName', args) + '\n'
  return meld + '\n'


def xpathZipLists(source, xpath_text_1, xpath_text_2):
  xpath_list_1 = xpathToList(source, xpath_text_1)
  xpath_list_2 = xpathToList(source, xpath_text_2)
  if xpath_list_1 and xpath_list_2:
    return list(zip(xpath_list_1, xpath_list_2))
  return []


def xpathAddress(source, xpath_text):
  xpath_list = xpathToList(source, xpath_text)
  spacing = xpath_list[0] + ' ' + xpath_list[2]
  return spacing if len(xpath_list) > 0 else None


def xpathRoomNumber(source, xpath_text):
  xpath_list = xpathToList(source, xpath_text)
  if len(xpath_list) > 1:
    return xpath_list[1]
  return None


def xpathPhoneNumber(source, xpath_text):
  phone_number = xpathToCleanString(source, xpath_text)
  if phone_number:
    return phone_number[6:]  # remove the phone number link tel://
  return None


def xpathEmail(source, xpath_text):
  email = xpathToCleanString(source, xpath_text)
  if email:
    return email[7:]  # remove the email link mailto:
  return None


def personalSite(faculty_data):
  if len(faculty_data['websites']) > 0:
    for page, pageName in faculty_data['websites']:
      if faculty_data['name'] in pageName:
        return page
  return None


def schooling(edu, degree, colleges, facultyID):
  meld = ''
  splitedu = edu.split(',')
  found = False
  options = []
  for college in colleges:
    if college in edu:
      args = [facultyID, uniqueID(college), degree]
      meld += lispStyle('schooling', args) + '\n'
      found = True
      break  # Exact match, look no further
    if len(splitedu) > 1:
      if splitedu[1] in college:
        options.append(college)
  if not found:
    if len(options) > 0:
      collegeID = uniqueID(options[0])
      args = [facultyID, collegeID, degree]
      meld += lispStyle('schooling', args) + '\n'
    elif len(options) > 1:
      for option in options:
        if splitedu[2] in option:
          collegeID = uniqueID(option)
          args = [facultyID, collegeID, degree]
          meld += lispStyle('schooling', args) + '\n'
          break
    extras = [{'text':'MIT',
           'id':'MassachusettsInstituteofTechnology'},
          {'text':'M.I.T.',
           'id':'MassachusettsInstituteofTechnology'},
            {'text':'UCLA',
           'id':'UniversityofCaliforniaLosAngeles'},
            {'text':'State University of New York, Stony Brook',
           'id':'StonyBrookUniversity'},  # SUNY schools are confusing
            {'text':'University of Illinois at Urbana',
           'id':'UniversityofIllinoisatUrbanaChampaign'},
            {'text':'UC Santa Barbara',
           'id':'UniversityofCaliforniaSantaBarbara'},
            {'text':'Aristotelian University of Thessaloniki',
           'id':'AristotleUniversityofThessaloniki'},
            {'text':'Bogazici Univeristy',
           'id':'BogaziciUniveristy'},  # BogaziçiÜniversitesi
            {'text':'Tel-Aviv University',
           'id':'TelAvivUniversity'},
            {'text':'University of Belgrade',
           'id':'UniversityofBelgrade'},  # not listed at all...
            {'text':'Brown Univesity',
           'id':'BrownUniversity'},
            {'text':'University of Sts. Kiril and Metodij',
           'id':'SaintsCyrilandMethodiusUniversityofSkopje'},  # not listed at all...
            {'text':'Indian Institute of Technology, Madras',
           'id':'IndianInstituteofTechnologyMadras'},
            {'text':'I am an Associate Professor at the EECS',
           'id':'Princeton University'}]
    for extra in extras:
      if extra['text'] in edu:
        args = [facultyID, extra['id'], degree]
        meld += lispStyle('schooling', args) + '\n'
        break
  return meld


def facultyMeld(colleges):
  meld = ''
  for faculty in scrapeFacultyData():
    meld += getFacultyOntologized(faculty, colleges)
  return meld.strip()

def main():
  print('scraping NU faculty...', end="", flush=True)
  faculty_meld = facultyMeld(scrapeColleges())
  toFile(faculty_meld, '../krf', 'faculty')
  print('finished.')

if __name__ == '__main__':
    main()