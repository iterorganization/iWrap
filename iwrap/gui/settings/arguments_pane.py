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
        combobox_frame = ttk.Frame(labelframe)
        combobox_frame.pack(fill=tk.X, side=tk.TOP, pady=5)

        # COMBOBOX
        self.combobox_values = ['IDS', 'HDC']
        ttk.Label(combobox_frame, text="Data type:").pack(fill=tk.X, side=tk.LEFT, padx=10)
        self.data_type_combobox = ttk.Combobox(combobox_frame, state='readonly')
        self.data_type_combobox['values'] = self.combobox_values
        self.data_type_combobox.current(0)
        self.data_type_combobox.pack(fill=tk.X, side=tk.RIGHT, expand=1, padx=10)

        # MAIN CONTENT FRAME
        main_content_frame = ttk.Frame(labelframe)
        main_content_frame.pack(fill=tk.BOTH, expand=1, padx=3, pady=3)

        # TABLE FRAME
        table_frame = ttk.Frame(main_content_frame)
        table_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, padx=3, pady=3)

        # BUTTONS FRAME
        buttons_frame = ttk.Frame(main_content_frame, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, padx=3, pady=3)
        buttons_frame_center = ttk.Frame(buttons_frame)
        buttons_frame_center.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

    def reload(self):
        pass

    def update_settings(self):
        pass
