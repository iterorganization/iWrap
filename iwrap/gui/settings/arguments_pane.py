import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class ArgumentsPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Arguments", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, pady=10, expand=1)

        # COMBOBOX FRAME
        combobox_frame = tk.Frame(labelframe, pady=5)
        combobox_frame.pack(fill=tk.X, side=tk.TOP)

        # COMBOBOX
        self.combobox_values = ['IDS', 'HDC']
        ttk.Label(combobox_frame, text="Data type:").pack(fill=tk.X, side=tk.LEFT, padx=10)
        self.data_type_combobox = ttk.Combobox(combobox_frame, state='readonly')
        self.data_type_combobox['values'] = self.combobox_values
        self.data_type_combobox.current(0)
        self.data_type_combobox.pack(fill=tk.X, side=tk.RIGHT, expand=1, padx=10)

    def reload(self):
        pass

    def update_settings(self):
        pass
