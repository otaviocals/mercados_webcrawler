#                                                                                                       #
#   Just Another Webscraper (J.A.W.)                                                                    #
#                                    v0.1                                                               #
#                                                                                                       #
#       written by Otavio Cals                                                                          #
#                                                                                                       #
#   Description: A webscrapper for downloading tables and exporting them to .csv files autonomously.    #
#                                                                                                       #
#########################################################################################################


#Required external modules: Selenium, BeautifoulSoup4,

from csv import writer, reader
from contextlib import closing
from selenium.webdriver import PhantomJS
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from pathlib import Path
from os.path import isdir
from os import makedirs, getcwd, remove
from sys import platform
from datetime import datetime, timedelta
from dateutil.parser import parse
from urllib.request import urlretrieve
from glob import glob
from zipfile import ZipFile
import sys

######################
# Webscrapping Stage #
######################

def GoogleApiScraper(folder, print_output = None, visual_output = None, phantom_path = ""):

    url="http://www2.susep.gov.br/menuestatistica/ses/principal.aspx"
    rows = []
    row = []
    append_to_csv = False
    current_os = platform

    if current_os.startswith("linux"):
        slash = "/"
    elif current_os.startswith("win32") or current_os.startswith("cygwin"):
        slash = "\\"
    elif current_os.startswith("darwin"):
        slash = "/"
    else:
        slash = "/"

    folder_data = folder+slash+"data"
    folder_logs = folder+slash+"logs"


#Checking Folders Existence

    if(not isdir(folder)):
        makedirs(folder)
    if(not isdir(folder_data)):
        makedirs(folder_data)
    if(not isdir(folder_logs)):
        makedirs(folder_logs)

#Creating date.log if there is no previous file

    if(not Path(folder_logs+slash+"date.log").is_file()):
        recorded_date = datetime(1970,1,1,0,0)
        with open(folder_logs+slash+"date.log","w") as f:
            print(recorded_date,file=f)
        print("No date.log detected. Creating file...")
        if not print_output == None:
            print_output.write("No date.log detected. Creating file...\n")
        if not visual_output == None:
            visual_output.append("No date.log detected. Creating file...\n")

#Loading date.log if it exists

    else:
        with open(folder_logs+slash+"date.log","r") as f:
            date_log_content = f.readlines()
            date_log_content = [x.strip() for x in date_log_content]
            recorded_date = parse(date_log_content[0])

#Getting Raw Data

    with closing(PhantomJS(phantom_path)) as browser:

        browser.implicitly_wait(50)
        tries = 0

    #Getting Newest Data Date

        browser.get(url)
        while tries <= 20:

            date_source = browser.find_elements_by_xpath("//form[@id=\"aspnetForm\"]/div[4]/table/tbody/tr/td/table[2]/tbody/tr[9]/td/font/div/span")
            date_string = date_source[0].text

            if len(date_string) > 0:
                break

            tries += 1
        last_date = parse(date_string)

    #Checking if Data is up to date

        if (recorded_date >= last_date):
            print("Data is up to date.")
            if not print_output == None:
                print_output.write("Data is up to date.\n")
            if not visual_output == None:
                visual_output.append("Data is up to date.\n")
            return False
        else:
            with open(folder_logs+slash+"date.log","w") as f:
                print(last_date,file=f)

        #Deleting Old Data

            print("New data detected! Removing old data...")
            if not print_output == None:
                print_output.write("New data detected! Removing old data...\n")
            if not visual_output == None:
                visual_output.append("New data detected! Removing old data...\n")

            for fl in glob(folder_data+slash+"*"):
                remove(fl)

        #Downloading Data

            print("Downloading new data...")
            if not print_output == None:
                print_output.write("Downloading new data...\n")
            if not visual_output == None:
                visual_output.append("Downloading new data...\n")

            data_source = browser.find_elements_by_xpath("//form[@id=\"aspnetForm\"]/div[4]/table/tbody/tr/td/table[2]/tbody/tr[9]/td/font/div/a")
            data_address = data_source[0].get_attribute("href")
            data_path = folder_data+slash+"zipped_data.zip"
            print(data_address)
            prct_achiev = [0,0,0,0,0,0,0,0,0,0]
            def dlProgress(count, blockSize, totalSize):

                if(prct_achiev[0] == 0 and (count*blockSize/totalSize) > 0.009 ):
                    prct_achiev[0] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/((elap_time.seconds*1000) +1))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("00% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[1] == 0 and (count*blockSize/totalSize) > 0.1 ):
                    prct_achiev[1] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("10% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[2] == 0 and (count*blockSize/totalSize) > 0.2 ):
                    prct_achiev[2] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("20% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[3] == 0 and (count*blockSize/totalSize) > 0.3 ):
                    prct_achiev[3] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("30% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[4] == 0 and (count*blockSize/totalSize) > 0.4 ):
                    prct_achiev[4] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("40% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[5] == 0 and (count*blockSize/totalSize) > 0.5 ):
                    prct_achiev[5] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("50% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[6] == 0 and (count*blockSize/totalSize) > 0.6 ):
                    prct_achiev[6] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("60% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[7] == 0 and (count*blockSize/totalSize) > 0.7 ):
                    prct_achiev[7] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("70% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[8] == 0 and (count*blockSize/totalSize) > 0.8 ):
                    prct_achiev[8] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("80% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

                if(prct_achiev[9] == 0 and (count*blockSize/totalSize) > 0.9 ):
                    prct_achiev[9] = 1
                    elap_time = datetime.now() - down_start
                    down_speed = int((count*blockSize)/(elap_time.seconds*1000))
                    eta_num = (totalSize)/(down_speed*60000)
                    eta = timedelta(minutes=int(eta_num), seconds = (eta_num - int(eta_num))*(60))
                    print("90% Downloaded. Download Speed: "+str(down_speed)+" kB/s Elapsed time: "+str(elap_time).split(".")[0]+" ETA: "+str(eta).split(".")[0])

            down_start = datetime.now()
            urlretrieve(data_address, data_path, reporthook = dlProgress)
            print("Download finished!")
            if not print_output == None:
                print_output.write("Download finished!\n")
            if not visual_output == None:
                visual_output.append("Download finished!\n")

        #Unzipping Data

            with ZipFile(data_path,"r") as zip_ref:
                zip_ref.extractall(folder_data)
            remove(data_path)
            return True


######################
#        Main        #
######################

if __name__ == "__main__":
    from sys import argv

    url = input("Enter url to scrap from:\n")
    folder = input("Enter folder to download to:\n")
    driver_path = input("Enter driver path:\n")

    print("Starting...")

    Webscraper(url,folder,phantom_path = driver_path)
