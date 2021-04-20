import tkinter as tk
from tkinter import Frame, ttk, messagebox
from tkinter import filedialog
from tkinter.constants import S, SEL_FIRST

from lxml import etree

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class CodeParametersPane(ttk.Frame, IWrapPane):
    """This pane is used to validate the XML file against the XSD schema file.

    XML file browser entry is located at the top of the pane.
    XSD file browser entry is located below the XML browse widget.
    Below both there is a button that triggers validations.
    After starting the validation process, a message box with information, 
    warning or error with the result of the verification will be displayed.

    Attributes:
        xml_browser (FileBrowser): Widget for browsing XML files.
        xsd_browser (FileBrowser): Widget for browsing XSD files.
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
        super().__init__(master)

        # XML file path browser dialog
        self.xml_browser = FileBrowserPane(self, file_type='xml', label_text="Code parameters file:")
        
        # XSD file path browser dialog
        self.xsd_browser = FileBrowserPane(self, file_type='xsd', label_text="Schema file:")

        # XML Validator object against XSD
        self._validator = XMLValidatorPane(self)

        #: The frame is set up with a padding 20 on the top
        self.configure(padding=(0, 20, 0, 0))

    # Parameters getter
    @property
    def parameters(self):
        """:obj: `str`: Get or set the current documentation text stored in ProjectSettings class.
        """
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        code_parameters = code_description.code_parameters
        parameters_file = code_parameters.parameters
        schema_file = code_parameters.schema

        return parameters_file, schema_file

    @parameters.setter
    def parameters(self, new_parameters: tuple[str, str] = ("", "")):
        try:
            parameters_file, schema_file = new_parameters
        except ValueError:
            raise ValueError("Pass an iterable with two items")
        else:
            self.update_settings(parameters_file, schema_file)

    def update_settings(self):
        self.xml_browser.update_settings()
        self.xsd_browser.update_settings()

    def reload(self):
        pass


class FileBrowserPane(ttk.Frame):
    """A universal FileBrowser class.

    Each FileBrowser object can search for a specific file extension, 
    as well as combinations of some or all of them.

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

    def __init__(self, master=None, file_type="", label_text="") -> None:
        """Initialize FileBrowser widget.

        Initialize an object composed of label, button, and dialog widgets. 
        It is possible to universally search for file types only depending 
        on the parameter specified by the file_type argument.

        Args:
            master (ttk.Frame, optional): A parent widget.
            file_type (str, optional): Describes what type of files
                should be searched for. Default - any type.
            label_text (str, optional): Title of the widget.
        """
        super().__init__(master)
        # Specify the file type
        self.file_type: tuple
        self.file_type_title: str
        self.file_type, self.file_type_title = self.define_file_type(file_type)

        # A label above widget
        self.label = ttk.Label(self, text=label_text)
        self.label.pack(side=tk.TOP, anchor=tk.SW, expand=True)

        # A button to browse files
        self.button = ttk.Button(self,
                                 text="Browse...",
                                 command=self.action_open)
        self.button.pack(side=tk.RIGHT, expand=False, fill=tk.X, padx=5)

        # Tk's StringVar to store path string. Get initial path from ProjectSettings().
        self.path = tk.StringVar(self, value=self.get_path_from_project(file_type))

        # An entry to display path dialog
        self.path_dialog = ttk.Entry(self, state='readonly', textvariable=self.path)
        self.path_dialog.pack(side=tk.LEFT, expand=True, fill=tk.X, padx=5)

        self.pack(expand=False, fill=tk.X, pady=5, ipady=5, padx=5, ipadx=5)

    # Parameters getter
    @property
    def parameters(self):
        """:obj: `str`: Get the current code parameters paths stored in ProjectSettings class.
        """
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        code_parameters = code_description.code_parameters
        parameters_file = code_parameters.parameters
        schema_file = code_parameters.schema

        return parameters_file, schema_file

    def get_path_from_project(self, file_type=""):
        """Get file path from master parameters stored in ProjectSettings ()"""

        parameters_file, schema_file = self.parameters

        # XML file type
        if file_type == 'xml':
            return parameters_file

        # XSD file type
        if file_type == 'xsd':
            return schema_file

        # Default file type
        return ""

    @staticmethod
    def define_file_type(file_type: str = ""):
        """Determines the file type.

        Determines the file type from the file_type parameter 
        to return a properly formatted object for the filedialog type.

        Args:
            file_type (str): String representation of the file type. 
        
        Returns:
            tuple: The return tuple composed of file extension object and the string representing it.
        """

        xml_extension = (("xml files", "*.xml"),)
        xsd_extension = (("xsd files", "*.xsd"),)
        any_extension = (('All files', '*.*'),)

        # XML file type
        if file_type == 'xml':
            return tuple((xml_extension, 'XML'))
        
        # XSD file type
        if file_type == 'xsd':
            return tuple((xsd_extension, 'XSD'))
        
        # Default file type
        return tuple((any_extension, 'Any'))

    def action_open(self):
        """Open system file dialog to browse files.
        
        Open system file dialog to browse and select files.
        The desired execution sets the path value based on the variable filename.

        Notes:
            If no path is selected, exits immediately.
        """
        filename = filedialog.askopenfilename(
                                            initialdir=None,
                                            title=f"Select {self.file_type_title} file",
                                            filetypes=self.file_type)
        if filename is None:
            return
        self.path.set(filename)

    def update_settings(self):
        # XML file type
        if self.file_type_title.lower() == 'xml':
            ProjectSettings.get_settings().code_description.code_parameters.parameters = self.path.get()

        # XSD file type
        if self.file_type_title.lower() == 'xsd':
            ProjectSettings.get_settings().code_description.code_parameters.schema = self.path.get()


    
class XMLValidatorPane(ttk.Frame):
    """A XML validator against XSD - xml schema.

    An object is a simple button widget which executes a validation process.
    It access previously loaded xml and xsd files, more precisely its path.
    It's able to parse both files using lxml package and run validation 
    of an xml file against the schema.

    Attributes:
        result (bool): Stores the result of the validation.
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

        # Button widget with the command to perform the validation.
        self.button = ttk.Button(self, text='Validate', command=self.validation_callback)
        self.button.pack(side=tk.TOP)

        # Configure the appearance.
        self.pack(side=tk.TOP, anchor=tk.CENTER, expand=False, pady=5, ipady=5, padx=5, ipadx=5)

    @staticmethod
    def correct_paths(file_1, file_2):
        """Subprocess to check that paths are correct.

        Returns:
            bool:   Returns False if any of path is incorrect. Returns True if every path is correct.
        """

        if file_1 == '' or file_2 == '':
            return False
        return True

    def validation_callback(self):
        """Callback method to perform the complete validation process."""
        xml = self.master.xml_browser.path.get()
        xsd = self.master.xsd_browser.path.get()

        # Check that the specified file paths are correct.
        if not self.correct_paths(xml, xsd):
            messagebox.showerror("WARNING! - Validation Error", f"Validation aborted:\n-INCORRECT PATH-")
            return

        # The validation process itself.
        self.result = self.validate_against_xsd(xml, xsd)

        # A message box with information about the validation result.
        messagebox.showinfo("Verification done", f"Validation result: \n{str(self.result).upper()}")

        # Overwrite master parameters stored in ProjectSettings().
        self.master.parameters = (xml, xsd)

    def validate_against_xsd(self, xml, xsd) -> bool:
        """Run xml validation process against given xsd.

        Args:
            xml (str): XML file path.
            xsd (str): XML schema file path.

        Returns:
            bool: True if the validation was performed correctly and the result is positive.
            False when validation fails.
        """

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
