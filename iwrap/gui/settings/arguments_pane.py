import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table


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
        table_frame = tk.Frame(main_content_frame, highlightbackground="black", highlightthickness=1)
        table_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, padx=3, pady=3)
        table_frame.columnconfigure(0, weight=1)
        #table_frame.columnconfigure(1, weight=1)
        #table_frame.columnconfigure(2, weight=1)
        table_frame.columnconfigure(3, weight=1)

        # BUTTONS FRAME
        buttons_frame = ttk.Frame(main_content_frame, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, padx=3, pady=3)
        buttons_frame_center = ttk.Frame(buttons_frame)
        buttons_frame_center.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # TABLE
        data = [
            ['type_example1', 'a', 'a', "Label_example"],
            ['type_example2', 'b', 'b', "Label_example"],
            ['type_example3', 'c', 'c', "Label_example"],
            ['type_example4', 'd', 'd', "Label_example"],
            ['type_example5', 'a', 'a', "Label_example"],
            ['type_example6', 'b', 'b', "Label_example"],
            ['type_example7', 'c', 'c', "Label_example"],
            ['type_example8', 'd', 'd', "Label_example"],
        ]
        columns = ["Type", "Input", "Output", "Label"]
        self.table = Table(data, columns, table_frame)

        # BUTTONS
        ttk.Button(buttons_frame_center, text="Add", width=10).pack(side=tk.TOP, expand=1, pady=10)
        ttk.Button(buttons_frame_center, text="Up", command=self.table.row_up_feature, width=10).pack(side=tk.TOP, expand=1)
        ttk.Button(buttons_frame_center, text="Down", command=self.table.row_down_feature, width=10).pack(side=tk.TOP, expand=1)
        ttk.Button(buttons_frame_center, text="Remove", command=self.table.delete_row, width=10)\
            .pack(side=tk.TOP, expand=1, pady=10)

    def reload(self):
        pass

    def update_settings(self):
        pass
