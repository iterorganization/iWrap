import tkinter as tk
from tkinter import ttk
import tkinter.filedialog

from iwrap.gui.generics import IWrapPane
from iwrap.gui.settings.language_specific_panes.language_panes_mgmt import LanguagePanesManager


class CodeSettingsPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="User code settings", borderwidth=2, relief="groove", height=100)
        labelframe.pack(fill=tk.X, pady=10)
        labelframe.grid_columnconfigure(1, weight=1)

        # LANGUAGE PANE
        self.selected_programming_language = 'Fortran'
        self.language_pane = ''
        self.pane = ''
        self.add_language_pane()

        # COMBOBOX
        ttk.Label(labelframe, text="Programming language:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.programming_language_combobox = ttk.Combobox(labelframe, state='readonly')
        self.programming_language_combobox['values'] = ('Fortran', 'CPP', 'Python')
        self.programming_language_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))
        self.programming_language_combobox.bind("<<ComboboxSelected>>", self.change_language_pane)

        # BROWSE BUTTON AND ENTRY FOR PATH
        ttk.Label(labelframe, text="Source Code:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.browse_text = tk.Entry(labelframe)
        self.browse_text.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(labelframe, text="Browse", command=self.on_click, width=10)\
            .grid(row=1, column=2, padx=10, pady=5)

    def change_language_pane(self, eventObject):
        self.selected_programming_language = self.programming_language_combobox.get()
        self.pane.pack_forget()
        self.add_language_pane()

    def add_language_pane(self):
        self.language_pane = LanguagePanesManager.get_language_pane(self.selected_programming_language)
        self.pane = self.language_pane(self)
        self.pane.pack(fill="both", expand="yes", pady=10)

    def reload(self):
        pass

    def on_click(self):
        filename = tk.filedialog.askopenfilename()
        self.browse_text.insert(tk.END, filename)
