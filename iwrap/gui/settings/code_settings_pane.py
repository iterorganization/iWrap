import logging
import tkinter as tk
from tkinter import ttk
import tkinter.filedialog
from tkinter import messagebox

from iwrap.generation_engine.engine import Engine
from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings
from iwrap.gui.settings.language_specific_panes.language_panes_mgmt import LanguagePanesManager
from iwrap.settings.language_specific.language_settings_mgmt import LanguageSettingsManager


class CodeSettingsPane(ttk.Frame, IWrapPane):
    """Code setting pane contains a combobox with programming languages, entry for code path, entry for code name
    and browse button for searching user files in the file dialog.

    Attributes:
        default_programming_language(str): Value for default programming language. Default to Fortran.
        labelframe(LabelFrame): Main label frame for user code settings. This label frame is a place for
         programming language combobox, code path entry, code name entry, and browse button.
        code_path(StringVar): Value for code path from filedialog or the YAML file.
        main(StringVar): Value for code name, can be added and changed by users or imported from the YAML file.
        selected_programming_language(StringVar): Value for programming language selected in combobox.
        programming_language_combobox(Combobox): Combobox contains programming languages from combobox_values. Enables
         user to select preferable programming language or value is selected automatically if the YAML file is imported.
        browse_text(Entry): A place for code path value. Users can search their filedialog and select code path using
         the browse button or the code path is added automatically if the YAML file is imported.
        main_text(Entry): An editable place for code name value. Value can be added and changed manually by users.
         The code name is added automatically if the YAML file is imported.
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
        self.main = tk.StringVar()
        self.init = tk.StringVar()
        self.finish = tk.StringVar()

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="User code settings", borderwidth=2, relief="groove", height=100)
        labelframe.pack(fill=tk.X, pady=10)
        labelframe.grid_columnconfigure(1, weight=1)

        # LANGUAGE
        self.selected_programming_language.set(CodeSettingsPane.default_programming_language)

        # LANGUAGE COMBOBOX
        ttk.Label(labelframe, text="Language:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.programming_language_combobox = ttk.Combobox(labelframe, state='readonly')
        self.programming_language_combobox['values'] = list(Engine().active_generator.code_languages)
        self.programming_language_combobox.current(0)
        self.programming_language_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # BROWSE BUTTON AND ENTRY FOR PATH
        ttk.Label(labelframe, text="Code path:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.browse_text = ttk.Entry(labelframe, textvariable=self.code_path)
        self.browse_text.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(labelframe, text="Browse...", command=self.on_click, width=10)\
            .grid(column=2, row=2, padx=10, pady=5)

        # SUBROUTINES LABEL FRAME
        labelframe_sub = ttk.LabelFrame(self, text="Subroutines", borderwidth=2, relief="groove", height=100)
        labelframe_sub.pack(fill=tk.X, pady=5)
        labelframe_sub.grid_columnconfigure(1, weight=1)

        # INIT
        ttk.Label(labelframe_sub, text="Init:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.main_text = ttk.Entry(labelframe_sub, textvariable=self.init)
        self.main_text.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))

        # MAIN
        ttk.Label(labelframe_sub, text="Main:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.main_text = ttk.Entry(labelframe_sub, textvariable=self.main)
        self.main_text.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))

        # FINISH
        ttk.Label(labelframe_sub, text="Finish:").grid(column=0, row=3, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.main_text = ttk.Entry(labelframe_sub, textvariable=self.finish)
        self.main_text.grid(column=1, row=3, padx=10, pady=5, sticky=(tk.W, tk.E))

    def update_settings(self, *args):
        """Update code_path, main and programming_language values in ProjectSettings.
        """
        code_description = ProjectSettings.get_settings().code_description
        code_description.programming_language = self.selected_programming_language.get()
        code_description.code_path = self.code_path.get()
        code_description.subroutines.main = self.main.get()
        code_description.subroutines.finish = self.finish.get()
        code_description.subroutines.init = self.init.get()

    def reload(self):
        """Reload entries and combobox values when the project settings are changed. If programming language from new
        project settings is not available in combobox warning message box will be shown and the default value of
        programming language will be selected in combobox.
        """
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        self.programming_language_combobox['values'] = list(Engine().active_generator.code_languages)
        programming_language = code_description.programming_language or CodeSettingsPane.default_programming_language
        code_path = code_description.code_path or ''
        main = code_description.subroutines.main or ''
        init = code_description.subroutines.init or ''
        finish = code_description.subroutines.finish or ''

        if programming_language.lower() not in [x.lower() for x in self.programming_language_combobox['values']]:
            programming_language = CodeSettingsPane.default_programming_language
            messagebox.showwarning("Warning", f"Unknown programming language. "
                                              f"The programming language set to "
                                              f"{CodeSettingsPane.default_programming_language}.")
        self.programming_language_combobox.set('')
        self.programming_language_combobox.set(programming_language.lower())

        self.browse_text.delete(0, tk.END)
        self.code_path.set(code_path)
        self.main.set(main)
        self.finish.set(finish)
        self.init.set(init)

        ProjectSettings.get_settings().code_description.programming_language = self.programming_language_combobox.get()

    def on_click(self):
        """Open the filedialog when the browse button is clicked and insert selected path to the browse_text entry.
        """
        filename = tk.filedialog.askopenfilename()
        if filename:
            self.code_path.set(filename)

