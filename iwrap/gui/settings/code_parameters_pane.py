import tkinter as tk
from tkinter import Frame, ttk, messagebox
from tkinter import filedialog
from tkinter.constants import S, SEL_FIRST

from typing import Tuple, Union, List

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class CodeParametersPane(ttk.Frame, IWrapPane):

    def __init__(self, master=None):
        """This pane is used to validate the XML file against the XSD schema file.

        XML file browser entry is located at the top of the pane.
        XSD file browser entry is located below the XML browse widget.
        Below both there is a button that triggers validation.
        After starting the validation process, a message box with information,
        warning or error with the result of the verification will be displayed.

        There are three widgets to initialize, a two file browsers 
        for XML and XSD files and third widget to execute validation.

        Args:
            master (ttk.Frame, optional): A parent widget.
        """
        super().__init__(master)

        # XML file path browser dialog
        self.xml_browser = CodeParameterBrowserPane(self, label_text="Code parameters file:", file_type="XML")

        # XSD file path browser dialog
        self.xsd_browser = CodeParameterBrowserPane(self, label_text="Schema file:", file_type="XSD")

        # XML Validator object against XSD
        _validator = XMLValidatorPane(self, xml=self.xml_browser.file_path, xsd=self.xsd_browser.file_path)

        #: The frame is set up with a padding 20 on the top
        self.configure(padding=(0, 20, 0, 0))

    def update_settings(self):
        """Force an update of ProjectSettings from CodeParameterBrowserPane.
        """
        self.xml_browser.update_settings()
        self.xsd_browser.update_settings()

    def reload(self):
        """Calls CodeParameterBrowserPane method to reload content.
        """
        self.xml_browser.reload()
        self.xsd_browser.reload()


class CodeParameterPath:
    _extension_description: Tuple[Tuple[Union[Tuple[str], None]]] = ((("All files", "*.*"), ),
                                                                     (("XML files", "*.xml"),),
                                                                     (("XSD files", "*.xsd"),))

    def __init__(self) -> None:
        """General file type class for not specified file extension.
        Attributes:
            path (str): Stores the path string.
            path_valid (bool): Indicates whether the file path has a valid construction.
        """
        self.path: str = tk.StringVar(value="")
        self.path_valid: bool = False
        self.file_type = self._extension_description[0]

    def is_path_correct(self, path="") -> bool:
        """Checks that the provided path is constructed correctly and is a string type.
         Sets path_valid (boolean) with check result.
        Returns: Bool
        """
        if not path or not isinstance(path, str):
            # Set the path validity flag to False and return False.
            self.path_valid = False
            return False
        # Set the path validity flag to True
        self.path_valid = True
        return True

    def update_settings(self) -> None:
        """Updates the code parameters fields in ProjectSettings().
        """
        pass

    def reload(self) -> None:
        """Loads the code parameters fields from ProjectSettings() to path variable.
        """
        pass

    @property
    def _project_settings(self):
        return ProjectSettings.get_settings().code_description.code_parameters


class XMLPath(CodeParameterPath):
    def __init__(self) -> None:
        super(XMLPath, self).__init__()
        self.file_type = self._extension_description[1]

    def update_settings(self) -> None:
        """Updates the code parameters fields in ProjectSettings().
        """
        super(XMLPath, self).update_settings()
        self._project_settings.parameters = self.path.get()

    def reload(self) -> None:
        """Loads the code parameters fields from ProjectSettings() to path variable.
        """
        super(XMLPath, self).reload()
        path_to_set = self._project_settings.parameters
        if not self.is_path_correct(path_to_set):
            return

        self.path.set(path_to_set)


class XSDPath(CodeParameterPath):
    def __init__(self) -> None:
        super(XSDPath, self).__init__()
        self.file_type = self._extension_description[2]

    def update_settings(self) -> None:
        super(XSDPath, self).update_settings()
        self._project_settings.schema = self.path.get()

    def reload(self) -> None:
        super(XSDPath, self).reload()
        path_to_set = self._project_settings.schema
        if not self.is_path_correct(path_to_set):
            return

        self.path.set(path_to_set)


class CodeParameterBrowserPane(ttk.Frame):

    def __init__(self, master, file_type: str, label_text: str = "") -> None:
        """A universal file browser class.
        Each CodeParameterBrowserPane object can search for a specific file extension.

        Attributes:
            path (CodeParameterPath): Value holder for path string.

        Initializes an object composed of label, button, and dialog widget.

        Args:
            master (ttk.Frame): A parent widget.
            label_text (str, optional): Title above the widget.
            file_type (str): Describes a file type reference.
        """
        super().__init__(master)
        if file_type == "XML":
            self.file_path = XMLPath()
        elif file_type == "XSD":
            self.file_path = XSDPath()
        else:
            self.file_path = CodeParameterPath()

        # A label above widget
        ttk.Label(self, text=label_text).pack(side=tk.TOP, anchor=tk.SW, expand=True)

        # A button to browse files
        button = ttk.Button(self,
                            text="Browse...",
                            command=self.action_open)
        button.pack(side=tk.RIGHT, expand=False, fill=tk.X, padx=5)

        # An entry to display path dialog
        path_dialog = ttk.Entry(self, state='readonly', textvariable=self.file_path.path)
        path_dialog.pack(side=tk.LEFT, expand=True, fill=tk.X, padx=5)

        self.pack(expand=False, fill=tk.X, pady=5, ipady=5, padx=5, ipadx=5)

    def action_open(self) -> None:
        """Open system file dialog to browse files.
        Notes:
            If no path is selected, exits immediately.
        """
        filename = filedialog.askopenfilename(
                    initialdir=None,
                    title=f"Select {self.file_path.file_type[0][0]}",
                    filetypes=self.file_path.file_type)
        if not self.file_path.is_path_correct(filename):
            return

        # Save loaded path.
        self.file_path.path.set(filename)
        self.update_settings()

    def update_settings(self) -> None:
        """To update, ProjectSettings() reads the path from the widget and writes to the file object.
        """
        self.file_path.update_settings()

    def reload(self) -> None:
        """Load data from ProjectSettings and set it as path.
        """
        self.file_path.reload()


class XMLValidatorPane(ttk.Frame):
    """A pane that contains an XML validator against an XSD - xml schema.

    Initializes a new widget as a button that invokes an external validation method.

    Notes:
        The validation ends with a pop-up message with information,
        warning or an error depending on the validation run.
    """

    def __init__(self, master, xml: CodeParameterPath, xsd: CodeParameterPath) -> None:
        """Initialize the button and all other necessary variables
        that will allow the XML file validation process to start.
        Args:
            master (ttk.Frame): A parent widget.
        """
        super().__init__(master)

        # Button widget with the command to perform the validation.
        button = ttk.Button(self, text='Validate', command=self.validation_callback)
        button.pack(side=tk.TOP)

        # Initialize files to validate
        self._xml_file: CodeParameterPath = xml
        self._xsd_file: CodeParameterPath = xsd

        # Configure the appearance.
        self.pack(side=tk.TOP, anchor=tk.CENTER, expand=False, pady=5, ipady=5, padx=5, ipadx=5)

    def validation_callback(self) -> None:
        """Callback method to perform the complete validation process."""

        # Check that the specified file paths are correct.
        if not (self._xml_file.path_valid and self._xsd_file.path_valid):
            messagebox.showerror("WARNING! - Validation Error", f"Validation aborted:\n-INCORRECT PATH-")
            return

        # The validation process itself.
        try:
            xml_path = self._xml_file.path.get()
            xsd_path = self._xsd_file.path.get()
            ProjectSettings.get_settings().code_description.code_parameters\
                .validate_xml(parameters_xml_path=xml_path, schema_xsd_path=xsd_path)
        except Exception as error:
            messagebox.showerror("Validation Error", f"The process encountered an error. Verify the input files!\n\n"
                                                     f"{error}")
            pass
        else:
            # A message box with information about the validation result.
            messagebox.showinfo("Verification done", f"Validation passed")
