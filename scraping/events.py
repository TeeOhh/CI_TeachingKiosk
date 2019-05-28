from datetime import datetime, timedelta, date
from selenium import webdriver
from lxml import html
import requests
import regex as re
import string
import os
import time

from helpers import *

def scrapeEventData():
  # IDENTIFIERS FOR NAVIGATING EVENT PAGE:
  EVENTS_PAGE = 'https://planitpurple.northwestern.edu'
  # list of events, can get href to event detail page
  EVENTS_LIST = '//ul[@class="events"]/li/a/@href'
  # attributes to look for on event details page
  EVENT_NAME    = '//div[@class="event_header"]/h2/text()'
  REOCCURRING   = '//p[@id="recurring"]'
  AUDIENCE_TEXT = '//span[text()[contains(.,"Audience")]]/../text()'
  CATEGORY_TEXT = '//span[contains(@class, "event_category")]/text()'
  CONTACT_TEXT  = '//span[text()[contains(.,"Contact")]]/../text()'
  CONTACT_MAIL  = '//span[text()[contains(.,"Contact")]]/../a/text()'
  WHERE_TEXT    = '//span[text()[contains(.,"Where")]]/../text()'
  GROUP_TEXT    = '//span[text()[contains(.,"Group")]]/../a/text()'
  WHEN_TEXT     = '//span[text()[contains(.,"When")]]/../text()'
  COST_TEXT     = '//span[text()[contains(.,"Cost")]]/../text()'
  # SCRAPE:
  today = date.today()
  oneweek = timedelta(days = 7)
  options = webdriver.ChromeOptions()
  options.add_argument('headless')
  driver = webdriver.Chrome(options=options)
  all_events = []
  for x in range(4):  # 8 weeks ~= 2 months
    today_search = '/#search=' + str(today) + '/0/1+5/1//week'
    # get base event listing website
    driver.get(EVENTS_PAGE + today_search)
    time.sleep(1)
    htmlContent = driver.page_source
    # htmlContent = requests.get(url).content
    mainTree = html.fromstring(htmlContent)
    # grab events from page as list,
    event_pages = mainTree.xpath(EVENTS_LIST)
    # loop over every event on this page and collect scraped event data
    used_events = []
    for event_page in event_pages:
      # setup event data for each event
      event = dict.fromkeys(['url', 'event_name', 'month', 'day', 'time',
                   'day_of_week', 'reoccurring', 'location',
                   'audience', 'contact', 'group', 'cost',
                   'date_time', 'ID', 'category'], None)
      event['url'] = EVENTS_PAGE + event_page
      # go to specific event detail page
      driver.get(event['url'])
      content = driver.page_source
      # content = requests.get(EVENTS_PAGE + event_page).content
      tree = html.fromstring(content)
      # scrape event data
      name = xpathToCleanString(tree, EVENT_NAME)
      clean_name = name.replace('“', '\\"').replace('"', '\\"')
      if name not in used_events:
        event['event_name'] = clean_name
        event['ID']         = uniqueID(event['event_name'])
        used_events.append(event['event_name'])
        event['reoccurring'] = xpathToExistsBool(tree, REOCCURRING)
        event['date_time']   = xpathToCleanString(tree, WHEN_TEXT)
        event['location']    = xpathToCleanString(tree, WHERE_TEXT)
        event['audience']    = xpathToCleanString(tree, AUDIENCE_TEXT)
        event['contact']     = xpathToCleanString(tree, CONTACT_TEXT)
        event['contact_mail']= xpathToCleanString(tree, CONTACT_MAIL)
        event['group']       = xpathToCleanString(tree, GROUP_TEXT)
        event['cost']        = xpathToCleanString(tree, COST_TEXT)
        event['category']    = xpathToCleanString(tree, CATEGORY_TEXT)
        # collect all event data
        all_events.append(event)
    today += oneweek
  driver.close()
  return all_events


def getEventOnotologized(event):
  times, year, month, day = dayMonthYear(event['date_time'])
  new_id = 'NUEvent-' + str(year) + '-' + str(month)[:3] + '-' + str(day)
  new_id += '-' + uniqueID(event['event_name'])[:30]
  meld = lispStyle('in-microtheory', ['(NUEventMtFn', new_id + ')'])
  meld += '\n' + isa(new_id, 'NUEvent') + '\n'
  date, duration = dateAndDuration(times, year, month, day)
  meld += lispStyle('dateOfEvent', [new_id, date]) + '\n'
  meld += lispStyle('durationOfEvent', [new_id, duration]) + '\n'
  temp_args = [new_id, '"' + event['event_name'] + '"']
  meld += lispStyle('eventName', temp_args) + '\n'
  if event['location']:
    temp_args = [new_id, '"' + event['location'] + '"']
    meld += lispStyle('eventLocale', temp_args) + '\n'
  temp_args = [new_id, '"' + event['group'] + '"']
  meld += lispStyle('eventHost', temp_args) + '\n'
  ################
  # NOT YET USED #
  ################
  con_name, con_phone = processEventContact(event['contact'])
  if con_name:
    temp_args = [new_id, '"' + con_name + '"']
    meld += lispStyle('eventHostContact', temp_args) + '\n'
  if con_phone:
    temp_args = [new_id, '"' + con_phone + '"']
    meld += lispStyle('eventHostContact', temp_args) + '\n'
  if event['contact_mail']:
    temp_args = [new_id, '"' + event['contact_mail'] + '"']
    meld += lispStyle('eventHostContact', temp_args) + '\n'
  ################
  audiences = event['audience'].split(' - ')
  temp_args = [new_id, 'NUPerson']
  meld += lispStyle('eventAudience', temp_args) + '\n'
  for audience in audiences:
    if audience == 'Faculty/Staff':
      temp_args = [new_id, 'NUFaculty']
      meld += lispStyle('eventAudience', temp_args) + '\n'
      temp_args = [new_id, 'NUStaff']
      meld += lispStyle('eventAudience', temp_args) + '\n'
    elif audience == 'Post Docs/Docs':
      temp_args = [new_id, 'NUPhDStudent']
      meld += lispStyle('eventAudience', temp_args) + '\n'
      temp_args = [new_id, 'NUMastersStudent']
      meld += lispStyle('eventAudience', temp_args) + '\n'
    elif audience == 'Student':
      temp_args = [new_id, 'NUStudent']
      meld += lispStyle('eventAudience', temp_args) + '\n'
      temp_args = [new_id, 'NUUndergraduate']
      meld += lispStyle('eventAudience', temp_args) + '\n'
    elif audience == 'Public':
      temp_args = [new_id, 'NUVisitor']
      meld += lispStyle('eventAudience', temp_args) + '\n'
    elif audience == 'Graduate Students':
      temp_args = [new_id, 'NUGraduateStudent']
      meld += lispStyle('eventAudience', temp_args) + '\n'
  ################
  if event['reoccurring']:
    temp_args = [new_id, 't']
    meld += lispStyle('eventReoccurring', temp_args) + '\n'
  ################
  if event['cost']:
    temp_args = [new_id, '"' + event['cost'] + '"']
    meld += lispStyle('eventCost', temp_args) + '\n'
  ################
  if event['category']:
    temp_args = [new_id, '"' + event['category'] + '"']
    meld += lispStyle('eventCategory', temp_args) + '\n'
  return meld + '\n'


def dayMonthYear(date_time):
  date = date_time.split(',')
  times = date[2].split()
  year = times[0]
  month, day = date[1].replace(',', '').split()
  return times, year, month, day


def dateAndDuration(times, year, month, day):
  date_text = '(YearFn ' + str(year) + ')'
  date_text = '(MonthFn ' + month + ' ' + date_text + ')'
  date_text = '(DayFn ' + day + ' ' + date_text + ')'
  times[1] = times[1].strip().replace('\\t', '').replace('\\n', '')
  if times[1] == 'All':
    duration = '(DaysDuration 1)'
  else:
    start_hr, start_min = splitHourMinute(times[1], times[2])
    date_text = '(HourFn ' + start_hr + ' ' + date_text + ')'
    date_text = '(MinuteFn ' + start_min + ' ' + date_text + ')'
    end_hr, end_min = splitHourMinute(times[4], times[5])
    start = start_hr + ':' + start_min
    end = end_hr + ':' + end_min
    duration = '(MinutesDuration ' + str(calcDuration(start, end)) + ')'
  return date_text, duration


def splitHourMinute(time, day_half):
  hours, minutes = time.split(':')
  if day_half == 'PM':
    hours = str(int(hours) + 12) if int(hours) != 12 else '12'
  else:
    hours = str(hours) if int(hours) != 12 else '0'
  return hours, minutes


def calcDuration(start_time, end_time):
  FMT = '%H:%M'
  start = datetime.strptime(start_time, FMT)
  end = datetime.strptime(end_time, FMT)
  duration = end - start
  return duration.seconds//60


def processEventContact(contact):
  printable = set(string.printable)
  filtered_contact = ''.join([x for x in contact if x in printable])
  phone_regex = re.compile('\d{3}.\d{3}.\d{4}')
  phone_match = phone_regex.findall(filtered_contact)
  if phone_match:
    name = filtered_contact.replace(phone_match[0], '').strip()
    return name, phone_match[0]
  return filtered_contact.strip(), None


def eventMeld():
  meld = ''
  for event in scrapeEventData():
    meld += getEventOnotologized(event)
  return meld.strip()


def main():
  print('scraping NU events from planitpurple...', end="", flush=True)
  events_meld = eventMeld()
  toFile(events_meld, '../krf', 'events')
  print('finished.')

if __name__ == '__main__':
    main()