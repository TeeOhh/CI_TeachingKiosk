from datetime import datetime, timedelta, date
from selenium import webdriver
from lxml import html
import requests
import regex as re
import string
import os
import time

def real_dirname():
    return os.path.dirname(os.path.realpath(__file__))


def xpathToList(source, xpath_text):
  xpath_list = source.xpath(xpath_text)
  return xpath_list if len(xpath_list) > 0 else None


def xpathToCleanString(source, xpath_text):
  xpath_list = xpathToList(source, xpath_text)
  if xpath_list:
    utf8_safe = str(' '.join(xpath_list))
    return utf8_safe.translate(str.maketrans('','','\t\n')).strip()
  else:
    return None


def xpathToExistsBool(source, xpath_text):
  return False if xpathToList(source, xpath_text) is None else True


def uniqueID(name):
  printable = set(string.printable)
  filtered_name = ''.join([x for x in name if x in printable])
  no_punc = filtered_name.translate(str.maketrans('','',string.punctuation))
  return no_punc.replace(' ', '')


def lispStyle(pred, args):
  return '(' + pred + ' ' + ' '.join(args) + ')'


def isa(arg1, arg2):
  return lispStyle('isa', [arg1, arg2])


def toFile(output_data, filepath, filename):
  with open('{}/{}.krf'.format(filepath, filename), 'w+') as f:
    f.write(output_data)
