import requests
from lxml import html

from helpers import *


def fetch_colleges():
    # list of all colleges and universities from some website...
    partial_url = 'https://www.4icu.org/reviews/index'

    all_colleges = []
    for page in ['{}{}.htm'.format(partial_url, str(i)) for i in range(28)]:
        tree = html.fromstring(requests.get(page).content)
        for college in tree.xpath('//tbody/tr/td/a[@href]/text()'):
            all_colleges.append(college)
    return all_colleges


def generate_college_krf(colleges):
    meld = '(in-microtheory CollegeNamesMt)\n'
    for college in colleges:
        if college:
            college = str(college).strip()
            college_id = unique_id(college)

            meld += isa(college_id, 'College') + '\n'
            args = [college_id, '"{}"'.format(college)]
            meld += lisp_style('prettyString', args) + '\n\n'
    return meld.strip()


def scrape_colleges():
    print('scraping colleges...', end="", flush=True)
    colleges = fetch_colleges()
    college_krf = generate_college_krf(colleges)
    to_file(college_krf, '../krf', 'colleges')
    print('finished.')


if __name__ == '__main__':
    scrape_colleges()
