import tkinter as tk
from tkinter import Frame, ttk, messagebox
from tkinter import filedialog
from tkinter.constants import S, SEL_FIRST

from lxml import etree

from iwrap.gui.generics import IWrapPane


class CodeParametersPane( ttk.Frame, IWrapPane ):
    """This pane is used to validate the XML file against the XSD schema file.

    XML file browser entry is located at the top of the pane.
    XSD file browser entry is located below the XML browse widget.
    Below both there is a button that triggers validations.
    After starting the validation process, a message box with information, 
    warning or error with the result of the verification will be displayed.

    Attributes:
        _xml_browser (FileBrowser): Widget for browsing XML files.
        _xsd_browser (FileBrowser): Widget for browsing XSD files.
        _validator (FileBrowser): Widget for validation processing.
    
    Notes:
        All CodeParametersPane attributes are considered protected 
        and should not be called explicitly.
    """

    def __init__(self, master=None):
        """Initialize widgets of the pane.

        There are three widgets to initialize, a two file browsers 
        for XML and XSD files and third widget to execute validation.

        Args:
            master (ttk.Frame, optional): A parent widget.
        """
        super().__init__( master )

        # XML file path browser dialog
        self._xml_browser = FileBrowserPane(self, file_type='xml', label_text="Code parameters file:")
        
        # XSD file path browser dialog
        self._xsd_browser = FileBrowserPane(self, file_type='xsd', label_text="Schema file:")

        # XML Validator object against XSD
        self._validator = XMLValidatorPane(self)

        #: The frame is set up with a padding 20 on the top
        self.configure(padding=(0, 20, 0 ,0))

    def update_settings(self):
        pass

    def reload(self):
        pass

class FileBrowserPane(ttk.Frame):
    """A universal FileBrowser class.

    Each FileBrowser object can search for a specific file extension, 
    as well as combinations of some or all of them.
    Acceptable file extensions are based on the internal FileTypes class.

    Attributes:
        file_type (tuple): Formatted parameter for filedialog filetype.
        file_type_title (str): Formatted parameter for filedialog title.
        label (ttk.Label): Label widget.
        button (ttk.Button): Button widget.
        path (tk.StringVar): Value holder for path string.
        path_dialog (ttk.Entry): Dialog box to display the path string.
    
    Notes:
        All FileBrowser attributes are preconfigured and packed. Therefore 
        their specification is not obligatory, 
        but it can be edited explicitly if necessary.
    """

    def __init__(self, master=None, file_type=None, label_text="") -> None:
        """Initialize FileBrowser widget.

        Initialize an object composed of label, button, and dialog widgets. 
        It is possible to universally search for file types only depending 
        on the parameter specified by the file_type argument. The actual 
        configuration is stored through an internal FileTypes object 
        and does not need to be called explicitly.

        Args:
            master (ttk.Frame, optional): A parent widget.
            file_type (FileTypes, optional): Describes what type of files 
                should be searched for. Default - any type.
            label_text (ttk.Label, optional): Title of the widget.
        """
        super().__init__(master)
        # Specify the file type
        self.file_type, self.file_type_title = self.define_file_type(file_type)
        
        # A label above widget
        self.label = ttk.Label(self, text=label_text)
        self.label.pack(side=tk.TOP, anchor=tk.SW, expand=True)

        # A button to browse files
        self.button = ttk.Button(self,
                                 text = "Browse...",
                                 command = self.action_open)
        self.button.pack(side=tk.RIGHT, expand=False, fill=tk.X, padx=5)

        # Tk's StringVar to store path string
        self.path = tk.StringVar(self)

        # An entry to display path dialog
        self.path_dialog = ttk.Entry(self, state='readonly', textvariable=self.path)
        self.path_dialog.pack(side=tk.LEFT, expand=True, fill=tk.X, padx=5)

        self.pack(expand=False, fill=tk.X, pady=5, ipady=5, padx=5, ipadx=5)

    def define_file_type(self, file_type):
        """Determines the file type.

        Determines the file type from the file_type parameter 
        to return a properly formatted object for the filedialog type.

        Args:
            file_type (str): String representation of the file type. 
        
        Returns:
            tuple: The return tuple composed of FileTypes object and the string representing it.
        """
        # XML file type
        if file_type == 'xml':
            return (self.FileTypes().xml, 'XML')
        
        # XSD file type
        if file_type == 'xsd':
            return (self.FileTypes().xsd, 'XSD')
        
        # Default file type
        return (self.FileTypes().all, 'Any')

    def action_open(self):
        """Open system file dialog to browse files.
        
        Open system file dialog to browse and select files.
        The desired execution sets the path value based on the variable filename.

        Notes:
            If no path is selected, exits immediately.
        """
        filename = filedialog.askopenfilename(  initialdir=None, 
                                                title=f"Select {self.file_type_title} file",
                                                filetypes=self.file_type )
        if filename is None:
            return
        self.path.set(filename)

    class FileTypes:
        """Describes the types of files to search..
        
        A FileType object is a tuple that can describe one or more file types. 
        Each item within a tuple is another tuple with a description 
        in the first position and a search pattern in the second.
        """

        def __init__(self) -> None:
            """Initialize a FileType object."""
            self._xml = tuple(("xml files","*.xml"))
            self._xsd = tuple(("xsd files","*.xsd"))
            self._all = tuple(('All files', '*.*'))

        @property
        def xml(self):
            """Get a XML file type description tuple."""
            return (self._xml,)

        @property
        def xsd(self):
            """Get a XSD file type description tuple."""
            return (self._xsd,)
        
        @property
        def all(self):
            """Get an any file type description tuple."""
            return (self._all,)

        def __getitem__(self, item):
            """Attribute getter."""
            return getattr(self, item)
        
        def __add__(self, other):
            """A simple way to combine two or more file type description tuples.
            """
            return (self[0],other[0])

    
class XMLValidatorPane(ttk.Frame):
    """A XML validator against XSD - xml schema.

    An object is a simple button widget which executes a validation process.
    It access previously loaded xml and xsd files, more precisely its path.
    It's able to parse both files using lxml package and run validation 
    of an xml file against the schema.

    Attributes:
        result (bool): Stores the result of the validation.
        files_to_validate (ValidationFiles): An object containing paths to validation files.
        button (ttk.Button): Widget that allows to run validation.
    
    Notes:
        The validation ends with a pop-up message with information, 
        warning or an error depending on the validation run.
    """

    def __init__(self, master=None) -> None:
        """Initialize XMLValidator widget.

        Initialize the button and all other necessary variables 
        that will allow the XML file validation process to start. 

        Args:
            master (ttk.Frame, optional): A parent widget.
        """

        super().__init__(master)

        # Validation result
        self.result: bool = False
        
        # Object of files to process validation
        self.files_to_validate = self.ValidationFiles(master)
        self.button = ttk.Button(self, text='Validate', command=self.validate_against_xsd)
        self.button.pack(side=tk.TOP)

        self.pack(side=tk.TOP, anchor=tk.CENTER, expand=False, pady=5, ipady=5, padx=5, ipadx=5)

    def validate_against_xsd(self, xml, xsd) -> bool:
        """Run xml validation process against given xsd."""

        # Parse xsd file:
        xmlschema_file = etree.parse(xsd)
        xmlschema = etree.XMLSchema(xmlschema_file)

        # Parse xml file:
        xml_file = etree.parse(xml)

        # Perform validation:
        try:
            validation_result = xmlschema.validate(xml_file)
        except (TypeError, Exception):
            messagebox.showerror("Validation Error", "The process encountered an error. Verify the input files!")
            return False
        return validation_result

    class ValidationFiles:
        """Stores validation files paths.

        Provides an easy interface for storing and reading validation file paths.
        It has a built-in mechanism to check if the given path is correct.

        Attributes:
            master (FileBrowser): The outside object that stores the paths.
        """
        def __init__(self, master) -> None:
            """Initialize the ValidationFiles object.
            
            Args:
                master (FileBrowser): A reference to the object that gives the paths.
            """
            self.master = master
            self._xml: str = master._xml_browser.path.get()
            self._xsd: str = master._xsd_browser.path.get()

        @property
        def xml(self):
            """Gets the XML file path."""
            return self._xml
        
        @property
        def xsd(self):
            """Gets the XSD file path."""
            return self._xsd
        
        def correct_path(self):
            """Subprocess to check that paths are correct.
            
            Returns:
                bool:   Returns False if any of path is incorrect. Returns True if every path is correct.
            """

            if self.xml == '' or self.xsd == '':
                return False
            return True

        def update(self):
            """Update all paths at once"""
            self._xml = self.master._xml_browser.path.get()
            self._xsd = self.master._xsd_browser.path.get()