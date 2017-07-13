#                                                                                                       #
#   Just Another Webscraper (J.A.W.)                                                                    #
#                                    v0.1                                                               #
#                                                                                                       #
#       written by Otavio Cals                                                                          #
#                                                                                                       #
#   Description: A webscrapper for downloading tables and exporting them to .csv files autonomously.    #
#                                                                                                       #
#########################################################################################################

#Required External Modules: cython, pygame, kivy
from kivy.app import App
from kivy.uix.label import Label
from kivy.uix.image import Image
from kivy.uix.textinput import TextInput
from kivy.uix.spinner import Spinner
from kivy.uix.button import Button
from kivy.uix.togglebutton import ToggleButton
from kivy.uix.filechooser import FileChooserListView
from kivy.uix.gridlayout import GridLayout
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.scrollview import ScrollView
from kivy.effects.scroll import ScrollEffect
from kivy.core.window import Window
from kivy.config import Config
from kivy.core.window import Window
from kivy.properties import ObjectProperty
from kivy.clock import Clock
from os.path import join, isdir, expanduser, isfile
from os import makedirs
from sys import platform
import subprocess
from pathlib import Path
from kivy.lang import Builder
from pylibs.file import XFolder
from pylibs.xpopup import XPopup
import pylibs.tools
from time import sleep
from unidecode import unidecode
from pyscripts.google_api_scraper import GoogleApiScraper
import time
import datetime
import gc
import sys
import os
import shutil
import webbrowser
import requests
import xml.etree.ElementTree as ET
import json
from pprint import pprint

######################
#    Pre-Settings    #
######################

#Setting configurations

Config.set("kivy","log_enable","0")
Config.set("kivy","log_level","critical")
Config.set("graphics","position","custom")
Config.set("graphics","top","10")
Config.set("graphics","left","10")
Config.set("graphics","width","450")
Config.set("graphics","height","750")
Config.set("graphics","fullscreen","0")
Config.set("graphics","borderless","0")
Config.set("graphics","resizable","0")
Config.write()

current_os = platform

try:
    import win32api
    win32api.SetDllDirectory(sys._MEIPASS)
except:
    pass

if current_os.startswith("linux"):
    slash = "/"
    phantom = "phantomjs/phantomjs"
elif current_os.startswith("win32") or current_os.startswith("cygwin"):
    slash = "\\"
    phantom = "phantomjs\\phantomjs.exe"
    import pylibs.win32timezone
elif current_os.startswith("darwin"):
    slash = "/"
    phantom = "phantomjs/phantomjs"
else:
    slash = "/"
    phantom = "phantomjs/phantomjs"


def resource_path(relative_path):
    try:
        base_path = sys._MEIPASS
    except Exception:
        base_path = os.path.abspath(".")
    return os.path.join(base_path,relative_path)

logo_path = resource_path("logo"+slash+"logo.png")
phantom_path = resource_path(phantom)
r_script_path = resource_path("rscripts")
r_libs_path = resource_path("rlibs")
kivi_app_path = resource_path("kivylibs"+slash+"app_screen.kv")
config_path = os.path.abspath(".")+slash+"config.txt"

#google_maps_api
api_key="AIzaSyCMpuUjGHTIMUvDeD-8NBSHzfiqf6Qi2UQ"
query_string="hipermercado+carrefour+am"

query_response = requests.get("http://maps.googleapis.com/maps/api/place/textsearch/xml?query="+query_string+"&key="+api_key)

#Building GUI

Builder.load_file(kivi_app_path)


######################
#    App Functions   #
######################

class AppScreen(GridLayout):

    default_folder = expanduser("~") + slash + "Documents"
    global sel_folder
    sel_folder = default_folder

#Calling webscraper

    def scrap(self,event):
        if(not isdir(sel_folder+slash+"logs")):
            makedirs(sel_folder+slash+"logs")
        if(not isdir(sel_folder+slash+"proc_data")):
            makedirs(sel_folder+slash+"proc_data")
        log_file = open((sel_folder+slash+"logs"+slash+"history.log"),"a",encoding="utf-8")

    #Starting scrap

        print("Starting at "+str(datetime.datetime.now()))
        log_file.write("Starting at "+str(datetime.datetime.now())+"\n")
        self.ids.log_output.text += "Starting at "+str(datetime.datetime.now())+"\n"

        start_time_seconds = time.time()
        self.ids.start_button.disabled = True

    #Running webscraper

        #str_output_logger = []
        #new_data = Webscraper(sel_folder,log_file,str_output_logger,phantom_path)
        #self.ids.log_output.text += "".join(str_output_logger)
        #gc.collect()

    #Running R scripts

        r_verify = shutil.which("Rscript")

        if(not r_verify == None):

            print("Obtaining Google Data...")
            log_file.write("Obtaining Google Data...\n")
            self.ids.log_output.text += "Obtaining Google Data...\n"

            log_file.close()

            r_output = subprocess.check_output(["Rscript",r_script_path+slash+"google_places_api.R", api_key, slash, sel_folder, r_script_path,r_libs_path, sel_folder+slash+"logs"+slash+"history.log"],universal_newlines=True)
            print(r_output)

            log_file = open((sel_folder+slash+"logs"+slash+"history.log"),"a",encoding="utf-8")

            print("Saving Data...")
            log_file.write("Saving Data...\n")
            self.ids.log_output.text += "Saving Data...\n"

        else:
            print("R compiler not found.")
            log_file.write("R compiler not found.\n")
            self.ids.log_output.text += "R compiler not found.\n"

    #Ending scrap

        self.ids.start_button.disabled = False
        end_time_seconds = time.time()
        elapsed_time = str(int(round(end_time_seconds - start_time_seconds)))

        print("Ending at "+str(datetime.datetime.now()))
        log_file.write("Ending at "+str(datetime.datetime.now())+"\n")
        self.ids.log_output.text += "Ending at "+str(datetime.datetime.now())+"\n"

        print("Total Elapsed Time: "+elapsed_time+" seconds.")
        log_file.write("Total Elapsed Time: "+elapsed_time+" seconds.\n")
        self.ids.log_output.text += "Total Elapsed Time: "+elapsed_time+" seconds.\n"

        print("Success!")
        log_file.write("Success!\n")
        self.ids.log_output.text += "Success!\n"

        log_file.close()

        self.ids.start_button.state = "normal"


#GUI functions

    def start(self,*args):
        global event
        if args[1] == "down":
            self.ids.folder_button.disabled = True
            event = Clock.schedule_once(self.scrap)
        if args[1] == "normal":
            self.ids.folder_button.disabled = False
            Clock.unschedule(event)

    def update_button(self,event):
        self.ToggleButton.text="Stop"

    def _open_link(self):
        webbrowser.open("http://www.calsemporium.com")

#Directory selection

    def _filepopup_callback(self, instance):
            if instance.is_canceled():
                return
            s = 'Path: %s' % instance.path
            s += ('\nSelection: %s' % instance.selection)
            global sel_folder
            self.ids.text_input.text = "Pasta selecionada:    " + instance.path
            sel_folder = instance.path

    def _folder_dialog(self):
        XFolder(on_dismiss=self._filepopup_callback, path=expanduser(u'~'))


#Setting window layout

    def __init__(self,**kwargs):
        super(AppScreen, self).__init__(**kwargs)

        Window.size = (450,750)
        Window.set_title("Webscraper Mercados")


        self.cols = 1
        self.size_hint = (None,None)
        self.width = 450
        self.height = 750
        self.icon = logo_path
        self.ids.ce_logo.source = logo_path



######################
#    Starting App    #
######################


class SUSEP(App):

    def build(self):
        self.icon = logo_path
        self.resizable = 0
        self.title = "Webscraper Mercados"
        self.log_enable = 0
        return AppScreen()

######################
#        Main        #
######################

if __name__ == "__main__" :
	SUSEP().run()
