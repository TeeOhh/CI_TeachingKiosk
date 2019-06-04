from datetime import datetime, timedelta, date
from selenium import webdriver
from lxml import html
import regex as re
import time

from helpers import *


def scrape_event_data():
    # IDENTIFIERS FOR NAVIGATING EVENT PAGE:
    events_page = 'https://planitpurple.northwestern.edu'

    # list of events, can get href to event detail page
    events_list = '//ul[@class="events"]/li/a/@href'

    # attributes to look for on event details page
    event_name = '//div[@class="event_header"]/h2/text()'
    reoccurring = '//p[@id="recurring"]'
    audience_text = '//span[text()[contains(.,"Audience")]]/../text()'
    category_text = '//span[contains(@class, "event_category")]/text()'
    contact_text = '//span[text()[contains(.,"Contact")]]/../text()'
    contact_mail = '//span[text()[contains(.,"Contact")]]/../a/text()'
    where_text = '//span[text()[contains(.,"Where")]]/../text()'
    group_text = '//span[text()[contains(.,"Group")]]/../a/text()'
    when_text = '//span[text()[contains(.,"When")]]/../text()'
    cost_text = '//span[text()[contains(.,"Cost")]]/../text()'

    # SCRAPE:
    today = date.today()
    oneweek = timedelta(days=7)
    options = webdriver.ChromeOptions()
    options.add_argument('headless')
    driver = webdriver.Chrome(options=options)
    all_events = []

    for x in range(4):  # 8 weeks ~= 2 months
        today_search = '/#search=' + str(today) + '/0/1+5/1//week'
        # get base event listing website
        driver.get(events_page + today_search)
        time.sleep(1)

        html_content = driver.page_source
        main_tree = html.fromstring(html_content)

        # grab events from page as list,
        event_pages = main_tree.xpath(events_list)

        # loop over every event on this page and collect scraped event data
        used_events = []
        for event_page in event_pages:
            # setup event data for each event
            event = dict.fromkeys(['url', 'event_name', 'month', 'day', 'time',
                                   'day_of_week', 'reoccurring', 'location',
                                   'audience', 'contact', 'group', 'cost',
                                   'date_time', 'ID', 'category'], None)
            event['url'] = events_page + event_page
            # go to specific event detail page
            driver.get(event['url'])
            content = driver.page_source
            tree = html.fromstring(content)

            # scrape event data
            name = xpath_to_clean_string(tree, event_name)
            clean_name = name.replace('“', '\\"').replace('"', '\\"')
            if name not in used_events:
                event['event_name'] = clean_name
                event['ID'] = unique_id(event['event_name'])
                used_events.append(event['event_name'])
                event['reoccurring'] = xpath_to_exists_bool(tree, reoccurring)
                event['date_time'] = xpath_to_clean_string(tree, when_text)
                event['location'] = xpath_to_clean_string(tree, where_text)
                event['audience'] = xpath_to_clean_string(tree, audience_text)
                event['contact'] = xpath_to_clean_string(tree, contact_text)
                event['contact_mail'] = xpath_to_clean_string(tree, contact_mail)
                event['group'] = xpath_to_clean_string(tree, group_text)
                event['cost'] = xpath_to_clean_string(tree, cost_text)
                event['category'] = xpath_to_clean_string(tree, category_text)

                # collect all event data
                all_events.append(event)

        today += oneweek

    driver.close()

    return all_events


def get_event_onotologized(event):
    times, year, month, day = day_month_year(event['date_time'])
    new_id = 'NUEvent-' + str(year) + '-' + str(month)[:3] + '-' + str(day)
    new_id += '-' + unique_id(event['event_name'])[:30]
    meld = lisp_style('in-microtheory', ['(NUEventMtFn', new_id + ')'])
    meld += '\n' + isa(new_id, 'NUEvent') + '\n'

    curr_date, duration = date_and_duration(times, year, month, day)
    meld += lisp_style('dateOfEvent', [new_id, curr_date]) + '\n'
    meld += lisp_style('durationOfEvent', [new_id, duration]) + '\n'

    temp_args = [new_id, '"' + event['event_name'] + '"']
    meld += lisp_style('eventName', temp_args) + '\n'

    if event['location']:
        temp_args = [new_id, '"' + event['location'] + '"']
        meld += lisp_style('eventLocale', temp_args) + '\n'

    temp_args = [new_id, '"' + event['group'] + '"']
    meld += lisp_style('eventHost', temp_args) + '\n'

    ################
    # NOT YET USED #
    ################
    con_name, con_phone = process_event_contact(event['contact'])
    if con_name:
        temp_args = [new_id, '"' + con_name + '"']
        meld += lisp_style('eventHostContact', temp_args) + '\n'
    if con_phone:
        temp_args = [new_id, '"' + con_phone + '"']
        meld += lisp_style('eventHostContact', temp_args) + '\n'
    if event['contact_mail']:
        temp_args = [new_id, '"' + event['contact_mail'] + '"']
        meld += lisp_style('eventHostContact', temp_args) + '\n'
    ################

    audiences = event['audience'].split(' - ')
    temp_args = [new_id, 'NUPerson']
    meld += lisp_style('eventAudience', temp_args) + '\n'

    for audience in audiences:
        if audience == 'Faculty/Staff':
            temp_args = [new_id, 'NUFaculty']
            meld += lisp_style('eventAudience', temp_args) + '\n'
            temp_args = [new_id, 'NUStaff']
            meld += lisp_style('eventAudience', temp_args) + '\n'
        elif audience == 'Post Docs/Docs':
            temp_args = [new_id, 'NUPhDStudent']
            meld += lisp_style('eventAudience', temp_args) + '\n'
            temp_args = [new_id, 'NUMastersStudent']
            meld += lisp_style('eventAudience', temp_args) + '\n'
        elif audience == 'Student':
            temp_args = [new_id, 'NUStudent']
            meld += lisp_style('eventAudience', temp_args) + '\n'
            temp_args = [new_id, 'NUUndergraduate']
            meld += lisp_style('eventAudience', temp_args) + '\n'
        elif audience == 'Public':
            temp_args = [new_id, 'NUVisitor']
            meld += lisp_style('eventAudience', temp_args) + '\n'
        elif audience == 'Graduate Students':
            temp_args = [new_id, 'NUGraduateStudent']
            meld += lisp_style('eventAudience', temp_args) + '\n'
    ################

    if event['reoccurring']:
        temp_args = [new_id, 't']
        meld += lisp_style('eventReoccurring', temp_args) + '\n'
    ################

    if event['cost']:
        temp_args = [new_id, '"' + event['cost'] + '"']
        meld += lisp_style('eventCost', temp_args) + '\n'
    ################

    if event['category']:
        temp_args = [new_id, '"' + event['category'] + '"']
        meld += lisp_style('eventCategory', temp_args) + '\n'

    return meld + '\n'


def day_month_year(date_time):
    curr_date = date_time.split(',')
    times = curr_date[2].split()
    year = times[0]
    month, day = curr_date[1].replace(',', '').split()
    return times, year, month, day


def date_and_duration(times, year, month, day):
    date_text = '(YearFn ' + str(year) + ')'
    date_text = '(MonthFn ' + month + ' ' + date_text + ')'
    date_text = '(DayFn ' + day + ' ' + date_text + ')'
    times[1] = times[1].strip().replace('\\t', '').replace('\\n', '')
    if times[1] == 'All':
        duration = '(DaysDuration 1)'
    else:
        start_hr, start_min = split_hour_minute(times[1], times[2])
        date_text = '(HourFn ' + start_hr + ' ' + date_text + ')'
        date_text = '(MinuteFn ' + start_min + ' ' + date_text + ')'
        end_hr, end_min = split_hour_minute(times[4], times[5])
        start = start_hr + ':' + start_min
        end = end_hr + ':' + end_min
        duration = '(MinutesDuration ' + str(calc_duration(start, end)) + ')'
    return date_text, duration


def split_hour_minute(curr_time, day_half):
    hours, minutes = curr_time.split(':')
    if day_half == 'PM':
        hours = str(int(hours) + 12) if int(hours) != 12 else '12'
    else:
        hours = str(hours) if int(hours) != 12 else '0'
    return hours, minutes


def calc_duration(start_time, end_time):
    fmt = '%H:%M'
    start = datetime.strptime(start_time, fmt)
    end = datetime.strptime(end_time, fmt)
    duration = end - start
    return duration.seconds // 60


def process_event_contact(contact):
    printable = set(string.printable)
    filtered_contact = ''.join([x for x in contact if x in printable])
    phone_regex = re.compile('\d{3}.\d{3}.\d{4}')
    phone_match = phone_regex.findall(filtered_contact)
    if phone_match:
        name = filtered_contact.replace(phone_match[0], '').strip()
        return name, phone_match[0]
    return filtered_contact.strip(), None


def generate_event_krf():
    meld = ''
    for event in scrape_event_data():
        meld += get_event_onotologized(event)
    return meld.strip()


def scrape_events():
    events_meld = generate_event_krf()
    to_file(events_meld, '../krf', 'events')


if __name__ == '__main__':
    scrape_events()
