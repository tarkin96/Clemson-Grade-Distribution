#import my libraries
import requests
from bs4 import BeautifulSoup
from requests.auth import HTTPBasicAuth
import selenium
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
import time
import csv
import os.path
import os
import getpass

from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By



# Ensures the login page is entirely loaded before login attempt. 
def login_load_wait(browser, timeout, name):

    try:
        element_present = EC.presence_of_element_located((By.ID, name))
        WebDriverWait(browser, timeout).until(element_present)
    except TimeoutException:
        print ("Timed out waiting for page to load")

# Ensures the page is entirely loaded before searching for elements by ID. 
def browser_load_wait(browser, timeout, name):
    try:
        element_present = EC.presence_of_element_located((By.ID, name))
        WebDriverWait(browser, timeout).until(element_present)

        element_present = EC.presence_of_element_located((By.ID, 'the-table'))
        WebDriverWait(browser, timeout).until(element_present)

    except TimeoutException:
        print ("Timed out waiting for page to load")


#the site to go to
main_site = 'https://virtssl.clemson.edu/oirssl/Grades/ReportGrades.cgi'

#the browser you want to use
#an example of my file path to my browser, which is chromedriver
browser = webdriver.Chrome('chromedriver')

#an example with chrome
#browser = webdriver.Chrome('your/path/to/chromedriver.exe')

#an example using firefox
#browser = webdriver.Firefox(executable_path='your/path/geckodriver.exe')

#put in your clemson log in info
username = input('Please enter your username : ')
password = getpass.getpass('Please enter your password: ')
print("\nLogging in")

login_info = {'username': username, 'password': password, 'donotcache': 'off'}

#go to main site
#if tyou are not logged in, this takes you to the login page
browser.get(main_site)
time.sleep(3) #allow the user to see stuff

#waits for page element to be found (driver, timeout length, html item ID)
login_load_wait(browser,20, 'submitButton')
login_load_wait(browser,20, 'username')
login_load_wait(browser,20, 'password')
login_load_wait(browser,20, 'donotcache')

if "idp.clemson.edu" in browser.current_url :
    #find the html elements on the login page
    username_html = browser.find_element_by_id("username")
    password_html = browser.find_element_by_id("password")
    checkbox_html = browser.find_element_by_id("donotcache")
    button_html = browser.find_element_by_id("submitButton")

    #put your username and password in the html elements
    username_html.send_keys(username)
    password_html.send_keys(password)

    #click the submit button
    button_html.click()

    #let the user see the login


# I think this needs to be here. Allows the page enough time to fully load. Ideally, we may have to  
    # implement a check to ensure that each page is fully loaded before parsing. 


new_dir_name = None;
# Ensure user wants to overwrite existing data.csv file. 
#if(os.path.isfile("data.csv")):
val = input('Overwrite data.csv? (y/n): ')
if(val == 'n' or val == 'N'):
    #print ("\nTerminating program")
    new_dir_name = input('Please enter a new directory name: ')
    if (not os.path.isdir(new_dir_name)) :
	    os.makedirs(new_dir_name)
    print("\nParsing tables\n\n")
    #exit()
else: 
    new_dir_name = "data"
    if (not os.path.isdir(new_dir_name)) :
        os.makedirs(new_dir_name)
    print("\nParsing tables\n\n")

main_depth = input('What do you want each CSV file to represent? Choose Term, College, or Dept (for none, press enter) ')
sec_depth = ""
tert_depth = ""
#if (not main_depth == "") :
 #   sec_depth = input('What do you want the parent folder to represent? Choose Term, College, or Dept (for none, press enter) ')
  #  if (not sec_depth == "") :
   #     tert_depth = input('What do you want the G-parent folder to represent? Choose Term, College, or Dept (for none, press enter) ')

print("\nParsing tables\n")


#get list of terms
browser_load_wait(browser,20, 'Term2')
terms = Select(browser.find_element_by_id('Term2'))
termlist = terms.options

#for every option in the terms options list
for index in range(0, len(termlist)):

    terms.select_by_index(index)
    browser_load_wait(browser,20, 'Budget_Ctr2')
    terms = Select(browser.find_element_by_id('Term2'))
    term_name = terms.first_selected_option.text

    #get list of colleges for the term
    colleges = Select(browser.find_element_by_id('Budget_Ctr2'))
    collegelist = colleges.options

    #for every college
    for index2 in range(0, len(collegelist)):
        
        colleges.select_by_index(index2)
        browser_load_wait(browser,20, 'Dept2')
        colleges = Select(browser.find_element_by_id('Budget_Ctr2'))
        college_name = colleges.first_selected_option.text

        #get list of departments in the college
        departments = Select(browser.find_element_by_id('Dept2'))
        deplist = departments.options
        #new_file_name = collegelist[index2].text + ".csv"
        
        # print("\rParsing:" + terms.first_selected_option.text + "  " + colleges.first_selected_option.text)
   
        
        for index3 in range(1, len(deplist)):
            departments.select_by_index(index3)
            browser_load_wait(browser,20, 'Dept2')
            departments = Select(browser.find_element_by_id('Dept2'))
            dept_name = departments.first_selected_option.text
            n = dept_name.find(" (")
            dept_name = dept_name[:n]
            
            parent_dir = ""
            if(main_depth == "") :
                new_file_name = "data.csv"
            elif (main_depth == "Term") :
                new_file_name = term_name + ".csv"
            elif (main_depth == "College") :
                #if (not os.path.exists(new_dir_name + "/" + term_name)) :
                    #os.makedir(new_dir_name + "/" + term_name)
                #parent_dir = term_name
                new_file_name = college_name + ".csv"
            else :
                #if (not os.path.exists(new_dir_name + "/" + term_name)) :
                    #os.mkdir(new_dir_name + "/" + term_name)
                #if (not os.path.exists(new_dir_name + "/" + term_name + "/" + college_name)) :
                    #os.mkdir(new_dir_name + "/" + term_name + "/" + college_name)
                #parent_dir = term_name + "/" + college_name
                new_file_name = dept_name + ".csv"

            #-------------------------------------------------------
            # parse the page
            new_text = browser.page_source.replace('\n', '')
            #soup = BeautifulSoup(browser.page_source, 'lxml')
            soup = BeautifulSoup(new_text, 'lxml')
            
            #if os.path.exists(new_dir_name + "/" + parent_dir + "/" + new_file_name):
            if os.path.exists(new_dir_name + "/" + new_file_name):
                append_or_write = "a"
            else:
                append_or_write = "w"

            tables = soup.find_all('table')[3]
      
            headers = []
            new_file_created = False;
            #if (not os.path.exists(new_dir_name + "/" + parent_dir + "/" + new_file_name)) :
            if (not os.path.exists(new_dir_name + "/" + new_file_name)) :
                new_file_created = True;
                for header in tables.find_all('th'):
                    if header.text != "Percentages":
                        headers.append(header.text)
                if (not (headers == [])):
                    headers[3] = "Professor"
                    headers[12] = "X"
                    headers[13] = "Pass"
                    headers[14] = "Fail"
                    headers = headers + ["Department", "College", "Term"]       
                        

            rows = []
            meta_row = [dept_name, college_name, term_name]
            for row in tables.find_all('tr'):
                data_row = [set.text for set in row.find_all('td')]
                if (len(data_row) >= 4):
                    m = data_row[3].find(" (")
                    data_row[3] = (data_row[3])[:m]
                    data_row[0] = data_row[0].replace(" ", "")
                if (not(data_row == [""] or data_row == [])):
                    data_row = data_row + meta_row
                rows.append(data_row)
                           
            #with open(new_dir_name + "/" + parent_dir + "/" + new_file_name, append_or_write, newline="\n") as f:
            with open(new_dir_name + "/" + new_file_name, append_or_write, newline="\n") as f:
                writer = csv.writer(f)
                #if(index == 0 and index2 == 0 and index3 == 1):
                if(new_file_created == True) :
                    writer.writerow(headers)
                  
                writer.writerows(row for row in rows if row)

            # -----------------------------------------

        browser_load_wait(browser,20, 'Budget_Ctr2')
        colleges = Select(browser.find_element_by_id('Budget_Ctr2'))
        #new_file_name = colleges.first_selected_option.text + ".csv"

    browser_load_wait(browser,20, 'Term2')
    terms = Select(browser.find_element_by_id('Term2'))


#get the html source code
#html = browser.page_source

#print the html source code
#print(html)


#exit the browser
browser.quit()


















