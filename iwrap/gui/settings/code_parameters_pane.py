import tkinter as tk
from tkinter import Frame, ttk, messagebox
from tkinter import filedialog
from tkinter.constants import S, SEL_FIRST

from typing import Tuple

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class CodeParametersPane(ttk.Frame, IWrapPane):
    """This pane is used to validate the XML file against the XSD schema file.

    XML file browser entry is located at the top of the pane.
    XSD file browser entry is located below the XML browse widget.
    Below both there is a button that triggers validation.
    After starting the validation process, a message box with information, 
    warning or error with the result of the verification will be displayed.
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
        self.xml_browser = FileBrowserPane(self, label_text="Code parameters file:", file_class=XMLFile)
        
        # XSD file path browser dialog
        self.xsd_browser = FileBrowserPane(self, label_text="Schema file:", file_class=XSDFile)

        # XML Validator object against XSD
        _validator = XMLValidatorPane(self)

        #: The frame is set up with a padding 20 on the top
        self.configure(padding=(0, 20, 0, 0))

    def update_settings(self):
        """Force an update of ProjectSettings from FileBrowserPanes.
        """
        self.xml_browser.update_settings()
        self.xsd_browser.update_settings()

    def reload(self):
        """Calls FileBrowserPanes method to reload content.
        """
        self.xml_browser.reload()
        self.xsd_browser.reload()


class File:
    """General file type class for not specified file extension.

        Attributes:
            path_valid (bool): Indicates whether the file path has a valid construction.


        """
    _EXTENSION: Tuple[Tuple[str, str], None] = (("All files", "*.*"),)
    _TITLE: str = "ANY"
    _PROJECT_SETTINGS = ProjectSettings.get_settings().code_description.code_parameters

    def __init__(self) -> None:
        """Initialize file extension object.
        """
        self._path: str = ""
        self.path_valid: bool = False

    @classmethod
    def info(cls) -> Tuple:
        """Returns a tuple with file extension matching browser format and file title."""
        return tuple((cls._EXTENSION, cls._TITLE))

    @classmethod
    def get_title(cls) -> str:
        """Returns file title."""
        return cls._TITLE

    def save_path(self, path: str = "") -> None:
        """Checks that path is correct, if yes saves to _path attribute.

        Args:
            path (str, optional): A path string to be stored.

        Note:
            Sets path_valid (boolean) with check result.
            Updates ProjectSettings()
        """
        self._path = path
        if not self.is_path_correct():
            return
        # Perform ProjectSettings() update
        self.update_settings()

    def get_path(self) -> str:
        """Returns stored file path string."""
        return self._path

    def is_path_correct(self) -> bool:
        """Checks that the file path is constructed correctly and is a string type.

        Returns: Bool

        Notes:
            Sets path_valid attribute.
        """
        if self._path == "" or not isinstance(self._path, str):
            # Set the path validity flag to False
            self.path_valid = False
            return False
        # Set the path validity flag to True
        self.path_valid = True
        return True

    def update_settings(self) -> None:
        """Updates the code parameters fields in ProjectSettings().
        """
        pass

    def load_settings(self) -> None:
        """Loads the code parameters fields from ProjectSettings() to PATH variable.
        """
        self.is_path_correct()


class XMLFile(File):
    """XML file type subclass."""
    _EXTENSION: Tuple[Tuple[str, str], None] = (("XML Files", "*.xml"),)
    _TITLE: str = "XML"

    def update_settings(self) -> None:
        self._PROJECT_SETTINGS.parameters = self._path

    def load_settings(self) -> None:
        self._path = self._PROJECT_SETTINGS.parameters
        super().load_settings()

    @classmethod
    def validate(cls) -> None:
        """Invokes XML validation method from ProjectSettings().
        """
        cls._PROJECT_SETTINGS.validate()


class XSDFile(File):
    """XSD file type subclass."""
    _EXTENSION: Tuple[Tuple[str, str], None] = (("XSD Files", "*.xsd"),)
    _TITLE: str = "XSD"

    def update_settings(self) -> None:
        self._PROJECT_SETTINGS.schema = self._path

    def load_settings(self) -> None:
        self._path = self._PROJECT_SETTINGS.schema
        super().load_settings()


class FileBrowserPane(ttk.Frame):
    """A universal FileBrowser class.

    Each FileBrowser object can search for a specific file extension, 
    as well as combinations of some or all of them.

    Attributes:
        file (File): File type object.
        file_type (tuple): Formatted parameter for filedialog filetype.
        file_type_title (str): Formatted parameter for filedialog title.
        path (tk.StringVar): Value holder for path string.
    
    Notes:
        All FileBrowser attributes are preconfigured and packed. Therefore 
        their specification is not obligatory, 
        but it can be edited explicitly if necessary.
    """

    def __init__(self, master=None, label_text="", file_class=File) -> None:
        """Initialize FileBrowser widget.

        Initialize an object composed of label, button, and dialog widgets. 
        It is possible to universally search for file types only depending 
        on the parameter specified by the file_type argument.

        Args:
            master (ttk.Frame, optional): A parent widget.
            label_text (str, optional): Title of the widget.
            file_class (File, optional): Describes class of files reference.
        """

        # Reference to a file class
        self.file = file_class()

        super().__init__(master)
        # Specify the file type
        self.file_type: tuple
        self.file_type_title: str
        self.file_type, self.file_type_title = file_class.info()

        # A label above widget
        ttk.Label(self, text=label_text).pack(side=tk.TOP, anchor=tk.SW, expand=True)

        # A button to browse files
        button = ttk.Button(self,
                            text="Browse...",
                            command=self.action_open)
        button.pack(side=tk.RIGHT, expand=False, fill=tk.X, padx=5)

        # Tk's StringVar to store path string. Get initial path from ProjectSettings().
        self.path = tk.StringVar(self, value=self.file.get_path())

        # An entry to display path dialog
        path_dialog = ttk.Entry(self, state='readonly', textvariable=self.path)
        path_dialog.pack(side=tk.LEFT, expand=True, fill=tk.X, padx=5)

        self.pack(expand=False, fill=tk.X, pady=5, ipady=5, padx=5, ipadx=5)

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

        # Save loaded path.
        self.file.save_path(filename)

        self.reload()

    def update_settings(self):
        """To update, ProjectSettings () reads the path from the widget and writes to the file object.
        """
        self.file.save_path(self.path.get())
        self.file.update_settings()

    def reload(self):
        """Load the path from ProjectSettings and set it on the widget.
        """
        # Load the path
        self.file.load_settings()

        # Update the text in the path dialog widget.
        self.path.set(self.file.get_path())

    
class XMLValidatorPane(ttk.Frame):
    """A pane that contains an XML validator against an XSD - xml schema.

    Initializes a new widget as a button that invokes an external validation method.

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

        # Button widget with the command to perform the validation.
        button = ttk.Button(self, text='Validate', command=self.validation_callback)
        button.pack(side=tk.TOP)

        # Initialize files to validate
        self._xml_file = XMLFile()
        self._xsd_file = XSDFile()

        # Configure the appearance.
        self.pack(side=tk.TOP, anchor=tk.CENTER, expand=False, pady=5, ipady=5, padx=5, ipadx=5)

    def validation_callback(self):
        """Callback method to perform the complete validation process."""

        # Loads the most current ProjectSettings() paths.
        self._xml_file.load_settings()
        self._xsd_file.load_settings()

        # Check that the specified file paths are correct.
        if not (self._xml_file.path_valid and self._xsd_file.path_valid):
            messagebox.showerror("WARNING! - Validation Error", f"Validation aborted:\n-INCORRECT PATH-")
            return

        # The validation process itself.
        try:
            XMLFile.validate()
        except Exception as error:
            messagebox.showerror("Validation Error", f"The process encountered an error. Verify the input files!\n\n"
                                                     f"{error}")
            pass
        else:
            # A message box with information about the validation result.
            messagebox.showinfo("Verification done", f"Validation passed")
