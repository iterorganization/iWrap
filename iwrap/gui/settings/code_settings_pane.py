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
        code_name(StringVar): Value for code name, can be added and changed by users or imported from the YAML file.
        selected_programming_language(StringVar): Value for programming language selected in combobox.
        language_pane(Frame): Frame dedicated for specific programming language pane selected in combobox.
        programming_language_combobox(Combobox): Combobox contains programming languages from combobox_values. Enables
         user to select preferable programming language or value is selected automatically if the YAML file is imported.
        browse_text(Entry): A place for code path value. Users can search their filedialog and select code path using
         the browse button or the code path is added automatically if the YAML file is imported.
        code_name_text(Entry): An editable place for code name value. Value can be added and changed manually by users.
         The code name is added automatically if the YAML file is imported.
    """
    default_programming_language = 'Fortran'

    def __init__(self, master=None):
        """Initialize the code settings pane.

        Args:
            master: Parent widget from Tkinter class. Default to None.
        """
        super().__init__(master)
        # self.combobox_values = list(Engine().active_generator.code_languages)
        self.code_path = tk.StringVar()
        self.selected_programming_language = tk.StringVar()
        self.code_name = tk.StringVar()

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="User code settings", borderwidth=2, relief="groove", height=100)
        labelframe.pack(fill=tk.X, pady=10)
        labelframe.grid_columnconfigure(1, weight=1)

        # LANGUAGE PANE
        self.selected_programming_language.set(CodeSettingsPane.default_programming_language)
        self.language_pane = None
        self.add_language_pane()

        # LANGUAGE COMBOBOX
        ttk.Label(labelframe, text="Language:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.programming_language_combobox = ttk.Combobox(labelframe, state='readonly')
        self.programming_language_combobox['values'] = list(Engine().active_generator.code_languages)
        self.programming_language_combobox.current(0)
        self.programming_language_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))
        self.programming_language_combobox.bind("<<ComboboxSelected>>", self.change_language_pane)

        # CODE NAME
        ttk.Label(labelframe, text="Code name:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.code_name_text = ttk.Entry(labelframe, textvariable=self.code_name)
        self.code_name_text.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))

        # BROWSE BUTTON AND ENTRY FOR PATH
        ttk.Label(labelframe, text="Code path:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.browse_text = ttk.Entry(labelframe, state='readonly', textvariable=self.code_path)
        self.browse_text.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(labelframe, text="Browse...", command=self.on_click, width=10)\
            .grid(column=2, row=2, padx=10, pady=5)

    def change_language_pane(self, event=None):
        """Update specific language pane when programming language in combobox is changed.

        Args:
            event: Combobox change value event object. Default to None.
        """
        if self.selected_programming_language.get() != self.programming_language_combobox.get():
            self.selected_programming_language.set(self.programming_language_combobox.get())
            self.language_pane.save_pane_settings()
            self.language_pane.pack_forget()
            self.add_language_pane()

    def add_language_pane(self):
        """Add specific language pane for selected programming language.
        """
        pane = LanguagePanesManager.get_language_pane(self.selected_programming_language.get())
        self.language_pane = pane(self)
        self.language_pane.pack(fill="both", expand="yes", pady=10)

    def update_settings(self, *args):
        """Update code_path, code_name and programming_language values in ProjectSettings.
        """
        code_description = ProjectSettings.get_settings().code_description
        code_description.programming_language = self.selected_programming_language.get()
        code_description.code_path = self.code_path.get()
        code_description.code_name = self.code_name.get()

        self.language_pane.update_settings()

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
        code_name = code_description.code_name or ''

        if programming_language not in self.programming_language_combobox['values']:
            programming_language = CodeSettingsPane.default_programming_language
            messagebox.showwarning("Warning", f"Unknown programming language. "
                                              f"The programming language set to "
                                              f"{CodeSettingsPane.default_programming_language}.")
        self.programming_language_combobox.set('')
        self.programming_language_combobox.set(programming_language)
        self.change_language_pane()

        self.browse_text.delete(0, tk.END)
        self.code_path.set(code_path)
        self.code_name.set(code_name)

        LanguageSettingsManager.set_settings(self.programming_language_combobox.get())
        self.language_pane.reload()

    def on_click(self):
        """Open the filedialog when the browse button is clicked and insert selected path to the browse_text entry.
        """
        filename = tk.filedialog.askopenfilename()
        if filename != '' and filename != ():
            self.code_path.set(filename)

