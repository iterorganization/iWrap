import logging
import tkinter as tk
from tkinter import ttk, messagebox
from tkinter import filedialog

from iwrap.common import utils
from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings
from iwrap.gui.settings.tooltip import ToolTip

from iwrap.generators.actor_generators.python_actor.resources.common.code_parameters_handlers.handler_factory import HandlerFactory
from iwrap.generators.actor_generators.python_actor.resources.common.code_parameters_handlers.parameters_handler_interface import ParametersHandlerInterface

class CodeParametersPane(ttk.Frame, IWrapPane):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """This pane is used to validate the XML file against the XSD schema file.

        parameters_path(StringVar): Value for parameters file path from filedialog or the YAML file.
        schema_path(StringVar): Value for parameters schema file path from filedialog or the YAML file.
        parameters_format(StringVar): Value for format of code parameters.

        parameters_path_entry(Entry): Entry for parameters file path.
        schema_path_entry(Entry): Entry for schema file path.
        format_combobox(Combobox): Combobox for parameters format.

        Below both there is a button that triggers validation.
        After starting the validation process, a message box with information,
        warning or error with the result of the verification will be displayed.

        Args:
            master (ttk.Frame, optional): A parent widget.
        """
        super().__init__(master)
        self.parameters_path = tk.StringVar()
        self.schema_path = tk.StringVar()
        self.parameters_format = tk.StringVar()

        self.grid_columnconfigure(1, weight=1)

        # PARAMETERS PATH AND BROWSE BUTTON
        ttk.Label(self, text="Parameters file:").grid(column=0, row=0, padx=10, pady=(15,5), sticky=(tk.W, tk.N))
        self.parameters_path_entry = ttk.Entry(self, textvariable=self.parameters_path)
        self.parameters_path_entry.grid(column=1, row=0, padx=10, pady=(15,5), sticky=(tk.W, tk.E))
        ttk.Button(self, text="Browse...", command=lambda: self.on_click_file(self.parameters_path), width=10).grid(
            column=2, row=0, padx=10, pady=(15,5))
        ToolTip(self.parameters_path_entry, 'parameters_file')

        # SCHEMA PATH AND BROWSE BUTTON
        ttk.Label(self, text="Schema file:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.schema_path_entry = ttk.Entry(self, textvariable=self.schema_path)
        self.schema_path_entry.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(self, text="Browse...", command=lambda: self.on_click_file(self.schema_path), width=10).grid(
            column=2, row=1, padx=10, pady=5)
        ToolTip(self.schema_path_entry, 'schema_file')

        # PARAMETERS FORMAT COMBOBOX
        ttk.Label(self, text="Format:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.format_combobox = ttk.Combobox(self, state='readonly', textvariable=self.parameters_format)
        self.format_combobox['values'] = ['legacy-xml', 'xml', 'json', 'namelist']
        self.format_combobox.current(0)
        self.format_combobox.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))
        ToolTip(self.format_combobox, 'parameters_format')

        # VALIDATE BUTTON
        validate_button = ttk.Button(self, text='Validate', command=self.validate_parameters)
        validate_button.grid(column=1, row=3, padx=10, pady=(15,5), sticky=(tk.W, tk.E))

    def validate_parameters(self):
        """Validate parameters against schema
        """
        # Check that the specified file paths are correct.
        if  (self.parameters_path.get() == '' or self.schema_path.get() == ''):
            messagebox.showerror("WARNING! - Validation Error", f"Validation aborted:\n-EMPTY PATH-")
            return

        # The validation process itself.
        try:
            parameters_handler : ParametersHandlerInterface = HandlerFactory.get_handler(self.parameters_format.get())
            parameters_handler.initialize(parameters_path=utils.resolve_path(self.parameters_path.get(), ProjectSettings.get_settings().root_dir_path),
                                          schema_path=utils.resolve_path(self.schema_path.get(), ProjectSettings.get_settings().root_dir_path))
            parameters_handler.validate()
        except Exception as error:
            try:
                # for jsonschema.exceptions.ValidationError
                message = error.message
            except AttributeError:
                # for other parameters formats errors
                message = str(error)
            messagebox.showerror("Validation Error", f"The process encountered an error. Verify the input files!\n\n"
                                                     f"{message}")
        else:
            # A message box with information about the validation result.
            messagebox.showinfo("Verification done", f"Validation passed")

    def update_settings(self):
        """Update settings in the ProjectSettings.
        """
        code_description = ProjectSettings.get_settings().code_description
        code_description.implementation.code_parameters.parameters = self.parameters_path.get()
        code_description.implementation.code_parameters.schema = self.schema_path.get()
        code_description.implementation.code_parameters.format = self.parameters_format.get()

    def reload(self):
        """
        """
        code_description = ProjectSettings.get_settings().code_description
        self.parameters_path.set(code_description.implementation.code_parameters.parameters)
        self.schema_path.set(code_description.implementation.code_parameters.schema)
        self.parameters_format.set(code_description.implementation.code_parameters.format)

    def on_click_file(self, path):
        """Open the filedialog when the browse button is clicked and insert selected path to the entry.
        """
        filename = tk.filedialog.askopenfilename()
        if filename:
            root_dir_path = ProjectSettings.get_settings().root_dir_path
            filename = utils.make_relative( filename, root_dir_path)
            path.set(filename)