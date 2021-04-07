import tkinter as tk
from tkinter import ttk
from tkinter import filedialog
from tkinter.constants import S, SEL_FIRST

from iwrap.gui.generics import IWrapPane


class CodeParametersPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        # XML file path browser dialog
        self._xml_browser = FileBrowser(self)
        
        # XSD file path browser dialog
        self._xsd_browser = FileBrowser(self)
        

    def reload(self):
        pass


class FileBrowser:
    def __init__(self, master=None) -> None:
        self.button = ttk.Button(master, text = "Browse XML File",command = self.action_open)
        self.button.pack()
    
    def action_open(self):
        filename = filedialog.askopenfilename(  initialdir=None, 
                                                title="Select XML file",
                                                filetypes=(("xml files","*.xml"),) )
        if filename is None:
            return
        print(filename)
