import tkinter as tk
from tkinter import ttk
import tkinter.filedialog

from iwrap.gui.generics import IWrapPane


class CodeSettingsPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)

        # LABEL FRAME
        self.labelframe = ttk.LabelFrame(self, text="User code settings")
        self.labelframe.pack(fill="both", expand="yes", pady=10)
        self.labelframe.grid_columnconfigure(1, weight=3)

        # COMBOBOX
        ttk.Label(self.labelframe, text="Programming language:").grid(column=0, row=0, padx=10, pady=5)
        self.compiler_combobox = ttk.Combobox(self.labelframe, state='readonly')
        self.compiler_combobox['values'] = (' Fortran', ' C++', ' Python')
        self.compiler_combobox.grid(column=1, columnspan=10, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

    def reload(self):
        pass
