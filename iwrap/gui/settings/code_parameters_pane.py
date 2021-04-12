import tkinter as tk
from tkinter import Frame, ttk, messagebox
from tkinter import filedialog
from tkinter.constants import S, SEL_FIRST

from iwrap.gui.generics import IWrapPane
from lxml import etree


class CodeParametersPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        # XML file path browser dialog
        self._xml_browser = FileBrowser(self, file_type='xml', label_text="Code parameters file:")
        
        # XSD file path browser dialog
        self._xsd_browser = FileBrowser(self, file_type='xsd', label_text="Schema file:")

        # XML Validator object against XSD
        self._validator = XmlValidator(self)

    def update_settings(self):
        pass

    def reload(self):
        pass

class FileBrowser(ttk.Frame):
    def __init__(self, master=None, file_type=None, label_text="") -> None:
        super().__init__(master)
        # Specify the file type
        self.file_type, self.file_type_title = self.__define_file_type(file_type)
        
        # A label above widget
        self.label = ttk.Label(self, text=label_text)
        self.label.pack(side=tk.TOP, anchor=tk.SW, expand=True)

        # A button to browse files
        self.button = ttk.Button(self, text = f"Browse {self.file_type_title} File", command = self.action_open)
        self.button.pack(side=tk.RIGHT, expand=False, fill=tk.X, padx=5)

        # Tk's StringVar to store path string
        self.path = tk.StringVar(self)

        # An entry to display path dialog
        self.path_dialog = ttk.Entry(self, state='readonly', textvariable=self.path)
        self.path_dialog.pack(side=tk.LEFT, expand=True, fill=tk.X, padx=5)

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
        self.path.set(filename)

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

    
class XmlValidator(ttk.Frame):
    def __init__(self, master=None) -> None:
        super().__init__(master)

        # Validation result
        self.result: bool = None
        
        # Object of files to process validation
        self.files_to_validate = self.ValidationFiles(master)
        self.button = ttk.Button(self, text='Validate', command=self._validate_against_xsd)
        self.button.pack(side=tk.TOP)

        self.pack(side=tk.TOP, anchor=tk.CENTER, expand=False, pady=5, ipady=5, padx=5, ipadx=5)
    
    def _validate_against_xsd(self):
        self.files_to_validate.update()
        xmlschema_file = etree.parse(self.files_to_validate.xsd)
        xmlschema = etree.XMLSchema(xmlschema_file)

        xml_file = etree.parse(self.files_to_validate.xml)
        validation_pass = xmlschema.validate(xml_file)
        
        # Save validation result
        self.result = validation_pass

        if not validation_pass:
            messagebox.showerror("Validation Error", f"Validation result:\n!!! {validation_pass} !!!")
            return
        messagebox.showerror("Validation Pass", f"Validation result: \n{validation_pass}")

    class ValidationFiles:
        def __init__(self, master) -> None:
            self.master = master
            self._xml: str = master._xml_browser.path.get()
            self._xsd: str = master._xsd_browser.path.get()

        @property
        def xml(self):
            return self._xml
        
        @property
        def xsd(self):
            return self._xsd
        
        def update(self):
            self._xml = self.master._xml_browser.path.get()
            self._xsd = self.master._xsd_browser.path.get()