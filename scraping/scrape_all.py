from colleges import scrape_colleges
from courses import scrape_courses
from faculty import scrape_faculty
from groups import scrape_groups
from cs_fields import scrape_cs_fields
from events import scrape_events


def run_scraper_code(code, text):
    print('scraping {} data...'.format(text), end='', flush=True)
    code()
    print('complete.')


def main():
    # colleges
    run_scraper_code(scrape_colleges, 'college')

    # courses
    run_scraper_code(scrape_courses, 'course')

    # faculty
    run_scraper_code(scrape_faculty, 'faculty')

    # groups
    run_scraper_code(scrape_groups, 'group')

    # cs_fields
    run_scraper_code(scrape_cs_fields, 'acm cs field')

    # events
    run_scraper_code(scrape_events, 'event')


if __name__ == '__main__':
    main()
