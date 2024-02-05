import logging
import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

import imas
from iwrap.generation_engine.engine import Engine
from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table
from iwrap.gui.widgets.table import Column
from iwrap.settings.project import ProjectSettings
from iwrap.gui.settings.tooltip import ToolTip


class ArgumentsPane( ttk.Frame, IWrapPane ):
    """The ArgumentsPane contains a combobox with values from available data types and a table with arguments. The buttons
    on the right side of the table allow to add a new row, modify the selected row, move the row up or down, and delete
    the selected row. The table is automatically filled with the argument values from the ProjectSettings.

    Attributes:
        arguments_settings ([dict]): The list of arguments from the ProjectSettings.
        columns ([Column]): The list of the Column class objects.
        table (Table): The table widget.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the ArgumentsPane class object.

        Args:
            master (ttk.Frame, None): The master frame.
        """
        super().__init__( master )
        self.arguments_settings = None

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Arguments", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, pady=10, expand=1)

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
        IDS = [ids.value for ids in list(imas.IDSName)] # pylint: disable=no-member

        self.columns = [Column(Column.TEXT, "Name", "Name"),
                        Column(Column.RADIOBUTTON, "Input", "Intent"),
                        Column(Column.RADIOBUTTON, "Output", "Intent"),
                        Column(Column.COMBOBOX, "Type", "Type", IDS)]

        # BUTTONS
        buttons = []
        add_button = ttk.Button(buttons_frame_center, text="Add...", width=10)
        add_button.pack(side=tk.TOP, expand=1)

        edit_button = ttk.Button(buttons_frame_center, text="Edit...", state='disabled', width=10)
        edit_button.pack(side=tk.TOP, expand=1, pady=10)
        buttons.append(edit_button)

        up_button = ttk.Button(buttons_frame_center, text="Up", state='disabled', width=10)
        up_button.pack(side=tk.TOP, expand=1)
        buttons.append(up_button)

        down_button = ttk.Button(buttons_frame_center, text="Down", state='disabled', width=10)
        down_button.pack(side=tk.TOP, expand=1)
        buttons.append(down_button)

        remove_button = ttk.Button(buttons_frame_center, text="Remove", state='disabled', width=10)
        remove_button.pack(side=tk.TOP, expand=1, pady=10)
        buttons.append(remove_button)

        # TABLE
        self.table = Table([], self.columns, table_frame, buttons)

        # BIND BUTTONS WITH TABLE METHODS
        add_button['command'] = lambda: self.table.add_row("argument")
        edit_button['command'] = lambda: self.table.edit_row("argument")
        up_button['command'] = self.table.row_up_feature
        down_button['command'] = self.table.row_down_feature
        remove_button['command'] = self.table.delete_row

    def reload(self):
        """Reload arguments settings and data type from the ProjectSettings, add arguments to the table. If data type
        from the ProjectSettings is not available in combobox warning message box will be shown and the default value
        of data type will be selected in combobox.
        """
        self.arguments_settings = ProjectSettings.get_settings().code_description.arguments
        self.set_data_to_table()

    def update_settings(self):
        """Update arguments and data type in the ProjectSettings.
        """
        arguments = self.get_data_from_table()
        ProjectSettings.get_settings().code_description.arguments = arguments

    def set_data_to_table(self):
        """Set data from arguments settings to the table.
        """
        table_data = []
        intent = {"IN": "Input", "OUT": "Output"}
        for argument in self.arguments_settings:
            table_data.append([argument.name, intent[argument.intent],
                               intent[argument.intent], argument.type])

        self.table.add_new_table_content(table_data)

    def get_data_from_table(self):
        """Return data from table.

        Returns: The list of data from each row from the table.
        """
        table_data = self.table.get_data_from_table()
        intent = {"Input": "IN", "Output": "OUT"}
        arguments = []
        for row in table_data:
            name = row['Name']
            row_type = row['Type']
            intent_value = intent[row['Intent']]
            arguments.append({'name': name, 'type': row_type, 'intent': intent_value})

        return arguments
