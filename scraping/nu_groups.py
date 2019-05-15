import os
import re

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup

url = 'https://northwestern.campuslabs.com/engage/organizations'

# create a new Chrome session
driver = webdriver.Chrome()
driver.implicitly_wait(30)
driver.get(url)