import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table
from iwrap.gui.widgets.table import Column
from iwrap.settings.project import ProjectSettings # nowe


class ArgumentsPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        self.arguments_settings = None

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

        # BUTTONS FRAME
        buttons_frame = ttk.Frame(main_content_frame, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, padx=3, pady=3)
        buttons_frame_center = ttk.Frame(buttons_frame)
        buttons_frame_center.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # TABLE
        self.columns = [Column(Column.TEXT, "Type"),
                        Column(Column.RADIOBUTTON, "Input"),
                        Column(Column.RADIOBUTTON, "Output"),
                        Column(Column.TEXT, "Label")]

        self.table = Table([], self.columns, table_frame)

        # BUTTONS
        ttk.Button(buttons_frame_center, text="Add...", command=self.table.add_row, width=10)\
            .pack(side=tk.TOP, expand=1)
        ttk.Button(buttons_frame_center, text="Edit...", command=self.table.edit_row, width=10)\
            .pack(side=tk.TOP, expand=1, pady=10)
        ttk.Button(buttons_frame_center, text="Up", command=self.table.row_up_feature, width=10)\
            .pack(side=tk.TOP, expand=1)
        ttk.Button(buttons_frame_center, text="Down", command=self.table.row_down_feature, width=10)\
            .pack(side=tk.TOP, expand=1)
        ttk.Button(buttons_frame_center, text="Remove", command=self.table.delete_row, width=10)\
            .pack(side=tk.TOP, expand=1, pady=10)

    def reload(self):
        self.arguments_settings = ProjectSettings.get_settings().code_description.arguments
        self.set_data_to_table()

    def update_settings(self):
        arguments = self.get_data_from_table()
        ProjectSettings.get_settings().code_description.arguments = arguments

    def set_data_to_table(self):
        table_data = []
        for argument in self.arguments_settings:
            table_data.append([argument['name'], argument['intent'] == 'IN', argument['intent'] == 'OUT', argument['type']])

        self.table.add_new_table(table_data, self.columns)

    def get_data_from_table(self):
        table_data = self.table.get_data_from_table()
        arguments = []
        for row in table_data:
            name = row['Type']
            row_type = row['Label']
            indent = ['IN' if row['Input'] is True else 'OUT'][0]
            arguments.append({'name': name, 'type': row_type, 'indent': indent})

        return arguments
