import logging
import tkinter as tk
from tkinter import ttk, messagebox
import tkinter.filedialog

from iwrap.common import utils
from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table
from iwrap.gui.widgets.table import Column
from iwrap.settings.settings.fortran_settings import ExtraLibraries
from iwrap.settings.settings.language_settings_mgmt import LanguageSettingsManager
from iwrap.settings.platform.pkg_config_tools import PkgConfigTools
from iwrap.settings.project import ProjectSettings


class FortranPane( ttk.Frame, IWrapPane ):
    """The FortranPane contains a compiler cmd entry, module path entry which enables the selection od module path from
    filedialog, two Combobox widgets that enables selection of MPI and OpenMP switch, and two tabs including
    PkgConfigPane, and LibraryPathPane. PkgConfigPane  contains the Table widget with system libraries and the
    LibraryPathPane frame contains the Table widget with library paths.

    Attributes:
        language (string): The language related to the class.
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        compiler_cmd (tk.StringVar()): The compiler cmd.
        openmp_switch_combobox (ttk.Combobox): The combobox enables switch openmp.
        mpi_combobox (ttk.Combobox): The combobox contains mpi values.
        module_path (tk.StringVar()): The value for include path.
        pkg_config_pane (PkgConfigPane): The PkgConfigPane class object.
        library_path_pane (LibraryPathPane): The LibraryPathPane class object.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    language = 'fortran'

    def __init__(self, master=None, language="fortran"):
        """Initialize the FortranPane class object.

        Args:
            master (ttk.Frame): The master frame.
        """
        super().__init__( master )
        FortranPane.language = language
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)

        if not ProjectSettings.get_settings().code_description.settings:
            ProjectSettings.get_settings().code_description.settings = self.settings

        # TABS FRAME
        tab_frame = ttk.Frame(self)
        tab_frame.pack(fill=tk.BOTH, side=tk.BOTTOM, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        settings_lib_tab = ttk.Frame(tab_control)
        libraries_lib_tab = ttk.Frame(tab_control)
        tab_control.add(settings_lib_tab, text="Settings:")
        tab_control.add(libraries_lib_tab, text="Extra libraries:")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.NW, pady=5)

        # LABEL FRAME
        labelframe = ttk.LabelFrame(settings_lib_tab, text="Settings", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, expand=1, pady=10)

        # FRAME
        frame = ttk.Frame(labelframe)
        frame.pack(fill=tk.X, side=tk.TOP, expand=0, anchor=tk.NW)
        frame.grid_columnconfigure(1, weight=1)

        # COMPILER CMD
        self.compiler_cmd = tk.StringVar()
        ttk.Label(frame, text="*Compiler cmd:").grid(column=0, row=2, padx=10, sticky=(tk.N, tk.W), pady=5)
        compiler_text = ttk.Entry(frame, textvariable=self.compiler_cmd)
        compiler_text.grid(column=1, row=2, padx=10, sticky=(tk.W, tk.E), pady=5)

        # FRAME MPI
        main_frame = ttk.Frame(labelframe)
        main_frame.pack(fill=tk.BOTH, side=tk.TOP, expand=0)

        # frame_batch = ttk.LabelFrame(main_frame, text="Batch", borderwidth=2, relief="groove")
        # frame_batch.pack(side=tk.RIGHT, fill=tk.X, expand=1, padx=10)
        # frame_batch.grid_columnconfigure(1, weight=1)

        # frame_mpi = ttk.LabelFrame(main_frame, text="MPI", borderwidth=2, relief="groove")
        # frame_mpi.pack(side=tk.LEFT, fill=tk.X, expand=1, padx=10)
        # frame_mpi.grid_columnconfigure(1, weight=1)

        self.openmp_switch_combobox = MpiCombo(frame, 0, 3, 10, "OpenMP switch:", self.settings.open_mp_switch)
        self.mpi_combobox = MpiCombo(frame, 0, 4, 10, "Mpi compiler cmd:", self.settings.mpi_compiler_cmd)
        # self.mpi_runner_combobox = BatchMpiCombo(frame_mpi, 0, 1, 5, "MPI runner:", self.settings.mpi.mpi_runner)
        # self.batch_default_queue_combobox = BatchMpiCombo(frame_batch, 0, 0, 5, "Batch default queue:", self.settings.batch.batch_default_queue)
        # self.batch_runner_combobox = BatchMpiCombo(frame_batch, 0, 1, 5, "Batch runner", self.settings.batch.batch_runner)

        # TABS FRAME
        tab_frame = ttk.Frame(libraries_lib_tab)
        tab_frame.pack(fill=tk.BOTH, side = tk.BOTTOM, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        sys_lib_tab = ttk.Frame(tab_control)
        cus_lib_tab = ttk.Frame(tab_control)
        tab_control.add(sys_lib_tab, text="pkg-config defined:")
        tab_control.add(cus_lib_tab, text="Path defined:")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.NW, pady=5)

        self.pkg_config_pane = PkgConfigPane(sys_lib_tab)
        self.library_path_pane = LibraryPathPane(cus_lib_tab)

    @staticmethod
    def handle_focus(event):
        """Handle focus event and set focus to the next widget.

        Args:
            event: The focus event.
        """
        event.widget.tk_focusNext().focus()

    def reload(self):
        """Reload system settings, set current value of compiler, module path, MPI and OpenMP switch.
        Call PkgConfigPane and LibraryPathPane reload methods.
        """
        self.settings = ProjectSettings.get_settings().code_description.settings

        self.compiler_cmd.set(self.settings.compiler_cmd)
        self.mpi_combobox.set(self.settings.mpi_compiler_cmd or "")
        self.openmp_switch_combobox.set(self.settings.open_mp_switch or "")

        self.library_path_pane.reload()
        self.pkg_config_pane.reload()

    def update_settings(self):
        """Update compiler, module path, MPI and OpenMP switch values in the ProjectSettings. Call PkgConfigPane
        and LibraryPathPane update_settings methods.
        """
        self.settings.compiler_cmd = self.compiler_cmd.get()
        self.settings.mpi_compiler_cmd = self.mpi_combobox.get()
        self.settings.open_mp_switch = self.openmp_switch_combobox.get()

        self.pkg_config_pane.update_settings()
        self.library_path_pane.update_settings()
        self.settings = ProjectSettings.get_settings().code_description.settings

    def save_pane_settings(self):
        """Save the data from a language pane to the dictionary using the LanguageSettingsManager.
        """

        compiler_cmd = self.compiler_cmd.get()
        open_mp_switch = self.openmp_switch_combobox.get()

        mpi_compiler_cmd = self.mpi_combobox.get()

        extra_lib = ExtraLibraries()
        pkg_configs = self.pkg_config_pane.get_data_from_table()
        library_paths = self.library_path_pane.get_list_of_paths()
        extra_lib.pkg_config_defined = pkg_configs
        extra_lib.path_defined = library_paths

        self.settings.from_dict({'compiler_cmd': compiler_cmd,
                                 'mpi_compiler_cmd': mpi_compiler_cmd,
                                 'open_mp_switch': open_mp_switch,
                                 'extra_libraries': extra_lib.to_dict()})


class MpiCombo:
    def __init__(self, frame, column, row, padx, text, settings):
        self.frame = frame
        self.column = column
        self.row = row
        self.padx = padx
        self.text = text
        self.settings = settings
        self.value = tk.StringVar()
        self.current_value = tk.StringVar()
        self.combobox = None
        self.add_combobox()

    def add_combobox(self):
        ttk.Label(self.frame, text=self.text).grid(column=self.column, row=self.row, padx=self.padx, pady=5, sticky=(tk.W, tk.N))
        self.value.set(self.settings)
        self.value.trace('w', self.change_current_value)
        self.current_value.set(self.value.get())
        self.combobox = ttk.Combobox(self.frame, textvar=self.value, width=15)
        self.combobox['values'] = [None]
        self.combobox.set(self.settings or "")
        self.combobox.grid(column=self.column+1, row=self.row, padx=self.padx, pady=5, sticky=(tk.W, tk.E))
        self.combobox.bind("<<ComboboxSelected>>", self.add_settings_to_combo)

    def reload(self):
        self.combobox.set(self.settings)

    def update_settings(self):
        self.settings = self.combobox.get()

    def add_settings_to_combo(self, *args):
        if self.current_value.get() not in self.combobox['value'] and self.current_value.get() is not '':
            self.combobox['values'] += (self.current_value.get(),)

    def get(self):
        return self.combobox.get()

    def set(self, value):
        self.combobox.set(value)

    def change_current_value(self, *args):
        if self.value.get() not in self.combobox['values']:
            self.current_value.set(self.value.get())


class PkgConfigPane:
    """The PkgConfigPane contains the Table widget with pkg configs. Add button opens new window and enables
    to add system library from list of available pkg config. Remove button enables to delete selected element from the
    Table.

    Attributes:
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        columns (list[Column]): The list of the Column class objects.
        table (Table): The table widget.
        master (ttk.Frame): The master frame.
        system_lib (PkgConfigTools): The PkgConfigTools class object.

    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the PkgConfigPane class object.

        Args:
            master (ttk.Frame): The master frame.
        """
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)
        self.pkg_config = PkgConfigTools()
        self.pkg_config.initialize()
        self.master = master

        # TABLE FRAME
        table_frame = ttk.Frame(master)
        table_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, anchor=tk.NW)

        # BUTTONS FRAMES
        buttons_frame = ttk.Frame(master, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, anchor=tk.NE)
        buttons_center_frame = ttk.Frame(buttons_frame)
        buttons_center_frame.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # BUTTONS
        add_button = ttk.Button(buttons_center_frame, text="Add...", width=10)
        add_button.pack(side=tk.TOP, expand=1, pady=5)
        info_button = ttk.Button(buttons_center_frame, text="Info...", width=10)
        info_button.pack(side=tk.TOP, expand=1, pady=5)
        remove_button = ttk.Button(buttons_center_frame, text="Remove", width=10)
        remove_button.pack(side=tk.TOP, expand=1, pady=5)

        # TABLE
        self.columns = [Column(Column.TEXT, "Name", "Name"),
                        Column(Column.TEXT, "Info", "Info"),
                        Column(Column.TEXT, "Description", "Description")]

        self.table = Table([], self.columns, table_frame, [remove_button, info_button])
        add_button['command'] = lambda: AddPkgConfigWindow(self)

        remove_button['command'] = self.table.delete_row
        info_button['command'] = lambda: SystemLibraryInfoWindow(self, self.master)
        self.__add_table_data()

    def __add_table_data(self):
        """Add pkg config to the table.
        """
        self.table.delete_data_from_table()

        if not self.settings.extra_libraries.pkg_config_defined:
            return

        data = []
        for sys_lib in self.settings.extra_libraries.pkg_config_defined:
            name = sys_lib
            system_lib_dict = self.pkg_config.get_pkg_config(name)
            if system_lib_dict is None:
                messagebox.showwarning("Warning", f"Unknown system library.")
                continue
            else:
                info = system_lib_dict['info']
                description = system_lib_dict['description']
                data.append([name, info, description])

        self.table.add_new_table_content(data)

    def add_row_to_table(self, data):
        self.table.add_rows([data])

    def get_data_from_table(self):
        """Get pkg config names from the table.

        Returns (list): The list with pkg configs from the table.
        """
        pkg_configs = self.table.get_data_from_table()
        pkg_config_name = []
        for pkg_config in pkg_configs:
            pkg_config_name.append(pkg_config['Name'])

        return pkg_config_name

    def reload(self):
        """Reload settings from the LanguageSettingsManager and add system libraries to the Table widget.
        """
        self.settings = ProjectSettings.get_settings().code_description.settings
        self.__add_table_data()

    def update_settings(self):
        """Update system library values in the ProjectSettings.
        """
        pkg_config = self.get_data_from_table()
        ProjectSettings.get_settings().code_description.settings.extra_libraries.pkg_config_defined = pkg_config


class SystemLibraryInfoWindow:
    def __init__(self, master=None, masterwindow=None):
        self.master = master

        self.lib_name = None
        self.lib_description = None
        self.get_lib_info()

        # WINDOW
        self.window = tk.Toplevel(masterwindow)
        self.window.geometry('700x500')
        self.window.resizable(False, False)
        self.window.title("System library info")
        self.window.focus_force()
        self.window.wait_visibility()
        self.window.grab_set()

        # FRAMES
        frame_lib_name = tk.Frame(self.window, height=30)
        frame_lib_name.pack(side=tk.TOP, fill=tk.X)
        frame_libs = tk.Frame(self.window, height=30)
        frame_libs.pack(side=tk.TOP, fill=tk.X, expand=1)
        frame_cflags = tk.Frame(self.window, height=30)
        frame_cflags.pack(side=tk.TOP, fill=tk.X)
        footer = tk.Frame(self.window, bd=1, relief=tk.SUNKEN, height=50)
        footer.pack(side=tk.BOTTOM, fill=tk.X)

        # LABELS
        tk.Label(frame_lib_name, text=f"{self.lib_name}")\
            .pack(side=tk.TOP, anchor=tk.SW, expand=True)
        tk.Label(frame_libs, text=f"pkg-config --libs {self.lib_name}")\
            .pack(side=tk.TOP, anchor=tk.SW, expand=True)
        tk.Label(frame_cflags, text=f"pkg-config --cflags {self.lib_name}")\
            .pack(side=tk.TOP, anchor=tk.SW, expand=True)

        # TEXT EDITORS
        self.text_editor_name = tk.Text(frame_lib_name, height=8, state='disabled')
        self.text_editor_name.pack(side=tk.TOP, expand=True, fill=tk.X)
        self.insert_text(self.text_editor_name, self.lib_description)

        self.text_editor_libs = tk.Text(frame_libs, height=8, state='disabled')
        self.text_editor_libs.pack(side=tk.TOP, expand=True, fill=tk.X)
        self.insert_text(self.text_editor_libs, self.master.pkg_config.get_linker_flags(self.lib_name))

        self.text_editor_cflags = tk.Text(frame_cflags, height=8, state='disabled')
        self.text_editor_cflags.pack(side=tk.TOP, expand=True, fill=tk.X)
        self.insert_text(self.text_editor_cflags, self.master.pkg_config.get_c_flags(self.lib_name))

        # CLOSE BUTTON
        remove_button = ttk.Button(footer, text="OK", command=self.window.destroy, width=10)
        remove_button.pack(padx=10, pady=10)

    def get_lib_info(self):
        selected_row = self.master.table.get_selected_row()
        table_data = self.master.table.get_data_from_table()
        selected_data = list(table_data[selected_row - 1].values())
        self.lib_name = selected_data[0]
        self.lib_description = selected_data[2]

    def insert_text(self, text_editor, text):
        text_editor.configure(state='normal')
        text_editor.insert('end', text)
        text_editor.configure(state='disabled')


class AddPkgConfigWindow:
    """Opens a new window with a table contains available pkg configs. The table can be filtered by user.

    Attributes:
        master (PkgConfigPane): The master pane.
        window (tk.Toplevel): The new window with system library table.
        table (Table): The table contains system library.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the AddPkgConfigWindow class object.

        Args:
            master (PkgConfigPane): The master pane.
        """
        self.master = master
        self.pkg_config = master.pkg_config

        # WINDOW
        self.window = tk.Toplevel(master.master)
        self.window.minsize(1000, 600)
        self.window.geometry('1000x600')
        self.window.resizable(width=False, height=True)
        self.window.title("Add system library")
        self.window.focus_force()
        self.window.grab_set()

        # FRAMES
        filter_frame = tk.Frame(self.window, height=50)
        filter_frame.pack(side=tk.TOP, fill=tk.X)
        content_frame = tk.Frame(self.window, height=300)
        content_frame.pack(side=tk.TOP, fill=tk.BOTH, expand=1)
        footer = tk.Frame(self.window, bd=1, relief=tk.SUNKEN, height=50)
        footer.pack(side=tk.BOTTOM, fill=tk.X)

        # FILTER BUTTON
        filter_value = tk.StringVar()
        tk.Entry(filter_frame, textvariable=filter_value, width=80).pack(side=tk.LEFT, expand=False, padx=20)
        filter_button = ttk.Button(filter_frame, text="Search", width=10)
        filter_button.pack(side=tk.LEFT, padx=10, pady=10)
        info_button = ttk.Button(filter_frame, text="Info...", width=10)
        info_button.pack(side=tk.LEFT, padx=10, pady=10)

        # TABLE
        system_lib_dict = master.pkg_config.system_lib_dict
        data = []
        for key, value in self.pkg_config.system_lib_dict.items():
            data.append([key, value['info'], value['description']])
        self.table = Table([], master.columns, content_frame, [info_button])
        self.table.add_rows(data)
        info_button['command'] = lambda: SystemLibraryInfoWindow(self, self.master.master)

        # BUTTONS
        add_button = ttk.Button(footer, text="Add", command=self.add_selected_data_to_table, width=10)
        add_button.pack(side=tk.RIGHT, padx=10, pady=10)
        remove_button = ttk.Button(footer, text="Cancel", command=self.window.destroy, width=10)
        remove_button.pack(side=tk.RIGHT, padx=10, pady=10)
        filter_button['command'] = lambda: self.table.filter_table(filter_value.get(), data)

    def add_selected_data_to_table(self):
        """Add selected system library to the master frame table.
        """
        selected_row = self.table.get_selected_row()
        table_data = self.table.get_data_from_table()
        if selected_row is not None:
            selected_data = list(table_data[selected_row - 1].values())
            self.master.add_row_to_table(selected_data)
        self.window.destroy()


class LibraryPathPane:
    """The PathPane contains the Table widget with the library paths. Add button enables to add library
    path from filedialog to the Table, remove button enables to delete selected library from the Table.

    Attributes:
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        table (Table): The Table contains custom libraries.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the CustomLibrariesPane class object.

        Args:
            master (ttk.Frame): The master frame.
        """
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)
        # LIBRARY PATH FRAME
        library_path_frame = tk.Frame(master)
        library_path_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, anchor=tk.NW)

        # BUTTONS FRAMES
        buttons_frame = ttk.Frame(master, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, anchor=tk.NE)
        buttons_center_frame = ttk.Frame(buttons_frame)
        buttons_center_frame.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # BUTTONS
        ttk.Button(buttons_center_frame, text="Add...", command=self.__add_on_click, width=10)\
            .pack(side=tk.TOP, expand=1, pady=5)
        remove_button = ttk.Button(buttons_center_frame, text="Remove", width=10)
        remove_button.pack(side=tk.TOP, expand=1, pady=5)

        # TABLE
        columns = [Column(Column.TEXT, "Library path", "Library path")]
        self.table = Table([], columns, library_path_frame, [remove_button])
        remove_button['command'] = self.table.delete_row
        self.__add_path_from_settings()

    def __add_path_from_settings(self):
        """Add custom libraries from the ProjectSettings to the Table widget.
        """
        data = []
        if self.settings.extra_libraries.path_defined is not None:
            for cus_lib in self.settings.extra_libraries.path_defined:
                data.append([cus_lib])
        self.table.add_new_table_content(data)

    def __add_on_click(self):
        """Open the filedialog and add selected path to the Table widget.
        """
        path = tk.filedialog.askopenfilename()
        if path not in ['', ()]:
            root_dir_path = ProjectSettings.get_settings().root_dir_path
            path = utils.make_relative( path, root_dir_path )
            self.table.add_rows([[path]])

    def get_list_of_paths(self):
        """Get the list of library paths from the table.

        Returns: The list of library paths.
        """
        data_table = self.table.get_data_from_table()
        return [data['Library path'] for data in data_table]

    def reload(self):
        """Reload library paths list from the LanguageSettingsManager and add it to the Table widget.
        """
        from iwrap.settings.project import ProjectSettings

        self.settings = ProjectSettings.get_settings().code_description.settings

        self.__add_path_from_settings()

    def update_settings(self):
        """Update library paths in the ProjectSettings.
        """
        library_paths = self.get_list_of_paths()
        ProjectSettings.get_settings().code_description.settings.extra_libraries.path_defined = library_paths

