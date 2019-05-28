from datetime import datetime, timedelta, date
from selenium import webdriver
from lxml import html
import requests
import regex as re
import string
import os
import time

from helpers import *

def scrapeColleges():
  # list of all colleges and universities from some website...
  PARTIAL  = 'https://www.4icu.org/reviews/index'
  all_colleges = []
  for page in [PARTIAL + str(i) + '.htm' for i in range(28)]:
    tree = html.fromstring(requests.get(page).content)
    for college in tree.xpath('//tbody/tr/td/a[@href]/text()'):
      all_colleges.append(college)
  return all_colleges


def meldColleges(colleges):
  meld = '(in-microtheory CollegeNamesMt)\n'
  for college in colleges:
    if college:
      college = str(college).strip()
      college_id = uniqueID(college)
      meld += isa(college_id, 'College') + '\n'
      args = [college_id, '"{}"'.format(college)]
      meld += lispStyle('prettyString', args) + '\n\n'
  return meld.strip()

def main():
  print('scraping colleges...', end="", flush=True)
  colleges = scrapeColleges()
  collegesMeld = meldColleges(colleges)
  toFile(collegesMeld, '../krf', 'colleges')
  print('finished.')

if __name__ == '__main__':
    main()
