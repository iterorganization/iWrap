import logging
import tkinter as tk
from tkinter import ttk
import tkinter.filedialog
from tkinter import messagebox

from iwrap.common import utils
from iwrap.generation_engine.engine import Engine
from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings
from iwrap.gui.settings.code_parameters_pane import CodeParametersPane
from iwrap.gui.settings.subroutines_pane import SubroutinesPane
from iwrap.gui.menu import MenuBar


class ImplementationPane(ttk.Frame, IWrapPane):
    """Code setting pane contains a combobox with programming languages, entry for code path, entry for code name
    and browse button for searching user files in the file dialog.

    Attributes:
        default_programming_language(str): Value for default programming language. Default to Fortran.
        code_path(StringVar): Value for code path from filedialog or the YAML file.
        selected_programming_language(StringVar): Value for programming language selected in combobox.
        programming_language_combobox(Combobox): Combobox contains programming languages from combobox_values. Enables
         user to select preferable programming language or value is selected automatically if the YAML file is imported.
        root_dir(StringVar): Value for rood dir path from filedialog or the YAML file.
        data_type_combobox(Combobox): Combobox contains data type. Enables user to select preferable data type.
        code_path_entry(Entry): Entry for code path.
        root_dir_entry(Entry): Entry for root dir.
        include_path(StringVar): StringVar for include path.
        subroutines_pane(SubroutinesPane): Pane contains subroutines.
        code_parameters_pane(CodeParametersPane): Pane contains code parameters.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    default_programming_language = 'fortran'

    def __init__(self, master=None):
        """Initialize the code settings pane.

        Args:
            master: Parent widget from Tkinter class. Default to None.
        """
        super().__init__(master)

        self.code_path = tk.StringVar()
        self.selected_programming_language = tk.StringVar()
        self.root_dir = tk.StringVar()
        self.data_type = None

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Implementation", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, side=tk.TOP, expand=0, anchor=tk.NW, pady=10)
        labelframe.grid_columnconfigure(1, weight=1)

        # LANGUAGE
        self.selected_programming_language.set(ImplementationPane.default_programming_language)

        # LANGUAGE COMBOBOX
        ttk.Label(labelframe, text="*Programming language:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.programming_language_combobox = ttk.Combobox(labelframe, state='readonly')
        self.programming_language_combobox['values'] = list(Engine().active_generator.code_languages)
        self.programming_language_combobox.current(0)
        self.programming_language_combobox.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))

        # DATA TYPE COMBOBOX
        ttk.Label(labelframe, text="*Data type:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.data_type_combobox = ttk.Combobox(labelframe, state='readonly')
        self.data_type_combobox['values'] = Engine().active_generator.code_data_types
        self.data_type_combobox.current(0)
        self.data_type_combobox.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))

        # BROWSE BUTTON AND ENTRY FOR PATH
        ttk.Label(labelframe, text="*Code path:").grid(column=0, row=4, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.code_path_entry = ttk.Entry(labelframe, textvariable=self.code_path)
        self.code_path_entry.grid(column=1, row=4, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(labelframe, text="Browse...", command=lambda: self.on_click_file(self.code_path), width=10).grid(column=2, row=4, padx=10, pady=5)

        # BROWSE BUTTON AND ENTRY FOR ROOT DIR
        ttk.Label(labelframe, text="Root dir:").grid(column=0, row=3, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.root_dir_entry = ttk.Entry(labelframe, textvariable=self.root_dir)
        self.root_dir_entry.grid(column=1, row=3, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(labelframe, text="Browse...", command=self.on_click_dir, width=10).grid(column=2, row=3, padx=10,
                                                                                           pady=5)

        # MODULE PATH
        self.include_path = tk.StringVar()
        self.include_path.set(ProjectSettings.get_settings().code_description.implementation.include_path or '')
        ttk.Label(labelframe, text="*Include path:").grid(column=0, row=5, padx=10, pady=5, sticky=(tk.W, tk.N))
        browse_button = ttk.Button(labelframe, text="Browse...", command=lambda: self.on_click_file(self.include_path), width=10)
        browse_button.bind("<FocusIn>", self.handle_focus)
        browse_text = ttk.Entry(labelframe, textvariable=self.include_path)
        browse_text.grid(column=1, row=5, padx=10, pady=5, sticky=(tk.W, tk.E))
        browse_button.grid(column=2, row=5, padx=10, pady=5, sticky=(tk.W, tk.E))

        # TABS FRAME
        tab_frame = ttk.Frame(self)
        tab_frame.pack(fill=tk.BOTH, side=tk.BOTTOM, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        subroutines_tab = ttk.Frame(tab_control)
        code_param_tab = ttk.Frame(tab_control)
        tab_control.add(subroutines_tab, text="Subroutines:")
        tab_control.add(code_param_tab, text="Code parameters:")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.SW, pady=5)

        self.subroutines_pane = SubroutinesPane(subroutines_tab)
        self.subroutines_pane.pack(fill=tk.BOTH, expand=1, anchor=tk.SW)
        self.code_parameters_pane = CodeParametersPane(code_param_tab)
        self.code_parameters_pane.pack(fill=tk.BOTH, anchor=tk.SW)

    def update_settings(self, *args):
        """Update settings in the ProjectSettings.
        """
        code_description = ProjectSettings.get_settings().code_description
        code_description.implementation.programming_language = self.programming_language_combobox.get()
        code_description.implementation.code_path = self.code_path.get()
        code_description.implementation.data_type = self.data_type_combobox.get()
        code_description.implementation.root_dir = self.root_dir.get()
        code_description.implementation.include_path = self.include_path.get()
        self.subroutines_pane.update_settings()
        self.code_parameters_pane.update_settings()

    def reload(self):
        """Reload entries and combobox values when the project settings are changed. If programming language from new
        project settings is not available in combobox warning message box will be shown and the default value of
        programming language will be selected in combobox.
        """
        code_description = ProjectSettings.get_settings().code_description

        self.programming_language_combobox['values'] = list(Engine().active_generator.code_languages)
        programming_language = code_description.implementation.programming_language or ImplementationPane.default_programming_language

        if programming_language.lower() not in [x.lower() for x in self.programming_language_combobox['values']]:
            programming_language = ImplementationPane.default_programming_language
            messagebox.showwarning("Warning", f"Unknown programming language. "
                                              f"The programming language set to "
                                              f"{ImplementationPane.default_programming_language}.")
        self.programming_language_combobox.set('')
        self.programming_language_combobox.set(programming_language.lower())

        self.data_type_combobox['values'] = Engine().active_generator.code_data_types
        if self.data_type not in self.data_type_combobox['values']:
            self.data_type_combobox.current(0)
        elif self.data_type not in self.data_type_combobox['values'] and self.data_type is not None:
            messagebox.showwarning("Warning", f"Unknown data type. "
                                              f"The data type set to "
                                              f"{self.data_type_combobox.get()}.")
        else:
            self.data_type_combobox.set(self.data_type)

        self.code_path_entry.delete(0, tk.END)
        self.code_path.set(code_description.implementation.code_path or '')
        self.include_path.set(code_description.implementation.include_path or "")

        self.root_dir_entry.delete(0, tk.END)
        self.root_dir.set(code_description.implementation.root_dir or '')
        self.subroutines_pane.reload()
        self.code_parameters_pane.reload()

        code_description.implementation.programming_language = self.programming_language_combobox.get()

    def on_click_file(self, path):
        """Open the filedialog when the browse button is clicked and insert selected path to the entry.
        """
        filename = tk.filedialog.askopenfilename()
        if filename:
            root_dir_path = ProjectSettings.get_settings().root_dir_path
            filename = utils.make_relative( filename, root_dir_path)
            path.set(filename)

    def on_click_dir(self):
        """Open the filedialog when the browse button is clicked and insert selected path to the entry.
        """
        old_dir = self.root_dir.get()
        dir_name = tk.filedialog.askdirectory()
        if dir_name:
            self.root_dir.set(dir_name)
            if old_dir == MenuBar.save_and_open_initialdir:
                MenuBar.save_and_open_initialdir = dir_name
            if old_dir == MenuBar.import_and_export_initialdir:
                MenuBar.import_and_export_initialdir = dir_name

    @staticmethod
    def handle_focus(event):
        """Handle focus event and set focus to the next widget.

        Args:
            event: The focus event.
        """
        event.widget.tk_focusNext().focus()