import tkinter, tkinter.ttk, logging, imas
from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Column
from iwrap.gui.widgets.table import Table
from iwrap.settings.project import ProjectSettings


class SubroutinePane(tkinter.ttk.Frame, IWrapPane):

    # class logger
    __logger = logging.getLogger(__name__ + '.' + __qualname__)

    def __init__(self, method_type, master=None):
        '''
        Arguments
            method_type (str): Must be one of the three following: init /
                main / finalize, for the reflection mechanism.
            master: Parent widget from Tkinter class. Defaults to None.
        Attributes:
            method_type (tk.StringVar()): A name of a subroutine that
                could be used to initialise or finalise the native code
                or the main subroutine that will be called from an actor.
        '''
        super().__init__(master)
        self.method_type = method_type
        self.method_name = tkinter.StringVar()
        self.subroutine = getattr(ProjectSettings.get_settings().code_description.implementation.subroutines, method_type)
        self.need_code_parameters = tkinter.BooleanVar()
        self.arguments_settings = None
        # LABEL FRAME
        labelframe = tkinter.ttk.Frame(self, height=100)
        labelframe.pack(fill=tkinter.BOTH, side=tkinter.TOP, expand=1, anchor=tkinter.NW, pady=10)
        labelframe.grid_columnconfigure(1, weight=1)
        # INIT / MAIN / FINALIZE
        tkinter.ttk.Label(labelframe, text='Name:').grid(column=0, row=1, padx=10, pady=5, sticky=(tkinter.W, tkinter.N))
        text = tkinter.ttk.Entry(labelframe, textvariable=self.method_name)
        text.grid(column=1, row=1, padx=10, pady=5, sticky=(tkinter.W, tkinter.E))
        # PASS CODE PARAMETERS
        tkinter.ttk.Label(labelframe, text='Need code parameters:').grid(column=0, row=2, padx=10, pady=5, sticky=(tkinter.W, tkinter.N))
        self.code_params_check = tkinter.Checkbutton(labelframe, variable=self.need_code_parameters)
        self.code_params_check.grid(column=1, row=2, padx=10, pady=5, sticky=(tkinter.W, tkinter.N))
        ### ARGUMENTS ###
        # LABEL FRAME
        labelframe = tkinter.ttk.LabelFrame(self, text='Arguments', borderwidth=2, relief='groove')
        labelframe.pack(fill=tkinter.BOTH, pady=10, expand=1)
        # MAIN CONTENT FRAME
        main_content_frame = tkinter.ttk.Frame(labelframe)
        main_content_frame.pack(fill=tkinter.BOTH, expand=1, padx=3, pady=3)
        # TABLE FRAME
        table_frame = tkinter.Frame(main_content_frame, highlightbackground='black', highlightthickness=1)
        table_frame.pack(fill=tkinter.BOTH, side=tkinter.LEFT, expand=1, padx=3, pady=3)
        # BUTTONS FRAME
        buttons_frame = tkinter.ttk.Frame(main_content_frame, width=100)
        buttons_frame.pack(fill=tkinter.BOTH, side=tkinter.RIGHT, expand=0, padx=3, pady=3)
        buttons_frame_center = tkinter.ttk.Frame(buttons_frame)
        buttons_frame_center.place(in_=buttons_frame, anchor='center', relx=.5, rely=.5)
        # TABLE
        IDS = [ids.value for ids in list(imas.IDSName)] # pylint: disable=no-member
        self.columns = [Column(Column.TEXT, 'Name', 'Name'),
                        Column(Column.RADIOBUTTON, 'Input', 'Intent'),
                        Column(Column.RADIOBUTTON, 'Output', 'Intent'),
                        Column(Column.COMBOBOX, 'Type', 'Type', IDS)]
        # BUTTONS
        buttons = []
        add_button = tkinter.ttk.Button(buttons_frame_center, text='Add...', width=10)
        add_button.pack(side=tkinter.TOP, expand=1)
        edit_button = tkinter.ttk.Button(buttons_frame_center, text='Edit...', state='disabled', width=10)
        edit_button.pack(side=tkinter.TOP, expand=1, pady=10)
        buttons.append(edit_button)
        up_button = tkinter.ttk.Button(buttons_frame_center, text='Up', state='disabled', width=10)
        up_button.pack(side=tkinter.TOP, expand=1)
        buttons.append(up_button)
        down_button = tkinter.ttk.Button(buttons_frame_center, text='Down', state='disabled', width=10)
        down_button.pack(side=tkinter.TOP, expand=1)
        buttons.append(down_button)
        remove_button = tkinter.ttk.Button(buttons_frame_center, text='Remove', state='disabled', width=10)
        remove_button.pack(side=tkinter.TOP, expand=1, pady=10)
        buttons.append(remove_button)
        # TABLE
        self.table = Table([], self.columns, table_frame, buttons)
        # BIND BUTTONS WITH TABLE METHODS
        add_button['command'] = lambda: self.table.add_row('argument')
        edit_button['command'] = lambda: self.table.edit_row('argument')
        up_button['command'] = self.table.row_up_feature
        down_button['command'] = self.table.row_down_feature
        remove_button['command'] = self.table.delete_row

    def update_settings(self, *args):
        '''
        Update settings in the ProjectSettings.
        '''
        self.subroutine.need_code_parameters = self.need_code_parameters.get()
        self.subroutine.arguments = self.get_data_from_table()
        self.subroutine.name = self.method_name.get()

    def reload(self):
        '''
        Reload init / main / finalize value when the project settings are changed.
        '''
        self.subroutine = getattr(ProjectSettings.get_settings().code_description.implementation.subroutines,
                                  self.method_type)
        self.method_name.set(self.subroutine.name or '')
        self.need_code_parameters.set(self.subroutine.need_code_parameters or False)
        self.arguments_settings = self.subroutine.arguments
        self.set_data_to_table()

    def set_data_to_table(self):
        '''
        Set data from arguments settings to the table.
        '''
        table_data = []
        intent = {'IN': 'Input', 'OUT': 'Output'}
        for argument in self.arguments_settings:
            table_data.append([argument.name, intent[argument.intent],
                               intent[argument.intent], argument.type])
        self.table.add_new_table_content(table_data)

    def get_data_from_table(self):
        '''
        Return data from table.
        Returns: The list of data from each row from the table.
        '''
        table_data = self.table.get_data_from_table()
        intent = {'Input': 'IN', 'Output': 'OUT'}
        arguments = []
        for row in table_data:
            name = row['Name']
            row_type = row['Type']
            intent_value = intent[row['Intent']]
            arguments.append({'name': name, 'type': row_type, 'intent': intent_value})
        return arguments
