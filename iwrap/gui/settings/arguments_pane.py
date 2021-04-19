import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class ArgumentsPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Arguments", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, pady=10, expand=1)
        labelframe.grid_columnconfigure(1, weight=1)

        # COMBOBOX
        self.combobox_values = ['IDS', 'HDC']
        ttk.Label(labelframe, text="Data type:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.data_type_combobox = ttk.Combobox(labelframe, state='readonly')
        self.data_type_combobox['values'] = self.combobox_values
        self.data_type_combobox.current(0)
        self.data_type_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

    def reload(self):
        pass

    def update_settings(self):
        pass
