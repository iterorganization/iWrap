import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class FortranPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )
        self.combobox_values = ['compiler1', 'compiler2', 'compiler3']

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Language specific settings", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, expand=1, pady=15)
        labelframe.grid_columnconfigure(1, weight=1)

        # COMBOBOX
        ttk.Label(labelframe, text="Compiler:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.compiler_combobox = ttk.Combobox(labelframe, state='readonly')
        self.compiler_combobox['values'] = self.combobox_values
        self.compiler_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

    def reload(self):
        pass
