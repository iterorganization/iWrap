import tkinter as tk
from tkinter import ttk
import tkinter.filedialog
from tkinter import messagebox

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings
from iwrap.gui.settings.language_specific_panes.language_panes_mgmt import LanguagePanesManager


class CodeSettingsPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)
        self.default_programming_language = 'Fortran'
        self.combobox_values = ['Fortran', 'CPP', 'Python']

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="User code settings", borderwidth=2, relief="groove", height=100)
        labelframe.pack(fill=tk.X, pady=10)
        labelframe.grid_columnconfigure(1, weight=1)

        # LANGUAGE PANE
        self.selected_programming_language = self.default_programming_language
        self.language_pane = ''
        self.pane = ''
        self.add_language_pane()

        # COMBOBOX
        ttk.Label(labelframe, text="Programming language:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.programming_language_combobox = ttk.Combobox(labelframe, state='readonly')
        self.programming_language_combobox['values'] = self.combobox_values
        self.programming_language_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))
        self.programming_language_combobox.bind("<<ComboboxSelected>>", self.change_language_pane)

        # BROWSE BUTTON AND ENTRY FOR PATH
        ttk.Label(labelframe, text="Source Code:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.browse_text = tk.Entry(labelframe)
        self.browse_text.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(labelframe, text="Browse", command=self.on_click, width=10)\
            .grid(row=1, column=2, padx=10, pady=5)

    def change_language_pane(self, eventObject=None):
        self.selected_programming_language = self.programming_language_combobox.get()
        self.pane.pack_forget()
        self.add_language_pane()

    def add_language_pane(self):
        self.language_pane = LanguagePanesManager.get_language_pane(self.selected_programming_language)
        self.pane = self.language_pane(self)
        self.pane.pack(fill="both", expand="yes", pady=10)

    def reload(self):
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description

        programming_language = code_description.programming_language or self.default_programming_language
        if programming_language not in self.combobox_values:
            programming_language = self.default_programming_language
            messagebox.showwarning("Warning", f"Unknown programming language. The default value ({self.default_programming_language}) has been set.")
        self.programming_language_combobox.set('')
        self.programming_language_combobox.set(programming_language)
        self.change_language_pane()

        code_path = code_description.code_path
        self.browse_text.delete(0, tk.END)
        self.browse_text.insert(tk.END, code_path)

    def on_click(self):
        filename = tk.filedialog.askopenfilename()
        self.browse_text.insert(tk.END, filename)
