import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class ArgumentsPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Arguments", borderwidth=2, relief="groove", height=200)
        labelframe.pack(fill=tk.X, pady=10)
        labelframe.grid_columnconfigure(0, weight=1)
        labelframe.grid_rowconfigure(1, weight=1)

        # COMBOBOX FRAME
        combobox_frame = tk.Frame(labelframe, pady=5)
        combobox_frame.grid(row=0, sticky="ew")
        combobox_frame.grid_rowconfigure(0, weight=1)
        combobox_frame.grid_columnconfigure(1, weight=1)

        # COMBOBOX
        self.combobox_values = ['IDS', 'HDC']
        ttk.Label(combobox_frame, text="Data type:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.data_type_combobox = ttk.Combobox(combobox_frame, state='readonly')
        self.data_type_combobox['values'] = self.combobox_values
        self.data_type_combobox.current(0)
        self.data_type_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

    def reload(self):
        pass

    def update_settings(self):
        pass
