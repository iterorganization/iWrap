import tkinter as tk
from tkinter import Frame, ttk
from tkinter import filedialog
from tkinter.constants import S, SEL_FIRST

from iwrap.gui.generics import IWrapPane


class CodeParametersPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        # XML file path browser dialog
        self._xml_browser = FileBrowser(self, file_type='xml').pack()
        
        # XSD file path browser dialog
        self._xsd_browser = FileBrowser(self, file_type='xsd').pack()

        # Any file path browser dialog
        self._any_browser = FileBrowser(self).pack()
        

    def update_settings(self):
        pass

    def reload(self):
        pass


class FileBrowser(ttk.Frame):
    def __init__(self, master=None, file_type=None) -> None:
        super().__init__(master)
        self.file_type, self.file_type_title = self.__define_file_type(file_type)
        
        # A button to browse files
        self.button = ttk.Button(self, text = f"Browse {self.file_type_title} File", command = self.action_open)
        self.button.pack(side='right', expand=False, fill=tk.X, padx=5)

        self.path_dialog = ttk.Entry(self, state='readonly')
        self.path_dialog.pack(side='left', fill=tk.X, expand=True)

        self.pack(expand=False, fill=tk.X, pady=5, ipady=5, padx=5, ipadx=5)
    
    def __define_file_type(self, file_type):
        # XML file type
        if file_type == 'xml':
            return (self.FileTypes().xml, 'XML')
        
        # XSD file type
        if file_type == 'xsd':
            return (self.FileTypes().xsd, 'XSD')
        
        # Default file type
        return (self.FileTypes().all, 'Any')

    def action_open(self):
        filename = filedialog.askopenfilename(  initialdir=None, 
                                                title=f"Select {self.file_type_title} file",
                                                filetypes=self.file_type )
        if filename is None:
            return
        print(filename)

    class FileTypes:
        def __init__(self) -> None:
            self._xml = tuple(("xml files","*.xml"))
            self._xsd = tuple(("xsd files","*.xsd"))
            self._all = tuple(('All files', '*.*'))

        @property
        def xml(self):
            return (self._xml,)

        @property
        def xsd(self):
            return (self._xsd,)
        
        @property
        def all(self):
            return (self._all,)
        
        def __add__(self, other):
            return (self[0],other[0])