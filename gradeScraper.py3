#   Author:     Sean Myers
#   Date:       February 27th, 2018
#   Purpose:    Script to collect grade data from clemson's website

#requests allow the script to send http requests
import requests
#pandas is an open source, library that will provide 
#data structures and data analysis tools for this script.
import pandas as pd
#csv library for converting collected data to csv
import csv
#Python library for pulling data from html and xml
from bs4 import BeautifulSoup
#a dictionary subclass which can remember the order in which content is added
from collections import OrderedDict
#selenium is a package used to automate web browser interactions from pyhton
from selenium import webdriver



url = r"view-source_https___virtssl.clemson.edu_oirssl_Grades_ReportGrades.cgi.html"

page = open(url)
data = page.read()

soup = BeautifulSoup(data, 'lxml')

tables = soup.find_all('table')[3]
#print(tables.text)

headers = [header.text for header in tables.find_all('th')]

rows = []

for row in tables.find_all('tr'):
    rows.append([val.text.encode('utf8') for val in row.find_all('td')])

with open('data.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(headers)
    writer.writerows(row for row in rows if row)