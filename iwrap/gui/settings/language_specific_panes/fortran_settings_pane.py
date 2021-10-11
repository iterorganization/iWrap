import tkinter as tk
from tkinter import ttk, messagebox
import tkinter.filedialog

from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table
from iwrap.gui.widgets.table import Column
from iwrap.settings.language_specific.fortran_settings import FortranSpecificSettings
from iwrap.settings.language_specific.fortran_settings import ExtraLibraries
from iwrap.settings.language_specific.language_settings_mgmt import LanguageSettingsManager
from iwrap.settings.platform.pkg_config_tools import PkgConfigTools
from iwrap.settings.project import ProjectSettings


class FortranPane( ttk.Frame, IWrapPane ):
    """The FortranPane contains a compiler cmd entry, module path entry which enables the selection od module path from
    filedialog, two Combobox widgets that enables selection of MPI and OpenMP switch, and two tabs including system
    library frame, and custom library frame. System library frame contains the Table widget with system libraries and
    the custom library frame contains the Table widget with custom libraries.

    Attributes:
        language (string): The language related to the class.
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        compiler_cmd (tk.StringVar()): The compiler cmd.
        openmp_switch_combobox (ttk.Combobox): The combobox enables switch openmp.
        mpi_combobox (ttk.Combobox): The combobox contains mpi values.
        module_path (tk.StringVar()): The value for include path.
        system_libraries_pane (SystemLibrariesPane): The SystemLibrariesPane class object.
        custom_libraries_pane (CustomLibrariesPane): The CustomLibrariesPane class object.
    """
    language = 'fortran'

    def __init__(self, master=None, language="fortran"):
        """Initialize the FortranPane class object.

        Args:
            master (ttk.Frame): The master frame.
        """
        super().__init__( master )
        FortranPane.language = language
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)

        if not ProjectSettings.get_settings().code_description.language_specific:
            ProjectSettings.get_settings().code_description.language_specific = self.settings

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Language specific settings", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, expand=1, pady=10)

        # FRAME
        frame = ttk.Frame(labelframe)
        frame.pack(fill=tk.BOTH, side=tk.TOP, expand=0, anchor=tk.NW)
        frame.grid_columnconfigure(1, weight=1)

        # COMPILER CMD
        self.compiler_cmd = tk.StringVar()
        ttk.Label(frame, text="Compiler cmd:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        compiler_text = ttk.Entry(frame, textvariable=self.compiler_cmd)
        compiler_text.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # MODULE PATH
        self.module_path = tk.StringVar()
        self.module_path.set(self.settings.include_path or '')
        ttk.Label(frame, text="Module path:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        browse_button = ttk.Button(frame, text="Browse...", command=self.open_filedialog, width=10)
        browse_button.bind("<FocusIn>", self.handle_focus)
        browse_text = ttk.Entry(frame, textvariable=self.module_path)
        browse_text.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))
        browse_button.grid(column=2, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))

        # COMBOBOX MPI
        ttk.Label(frame, text="MPI:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.mpi_combobox = ttk.Combobox(frame, state='readonly')
        self.mpi_combobox['values'] = ["True", "False", "None"]
        self.mpi_combobox.set([self.settings.mpi if self.settings.mpi not in [None, False, ''] else "None"])
        self.mpi_combobox.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))

        # COMBOBOX OpenMP switch
        ttk.Label(frame, text="OpenMP switch:").grid(column=0, row=3, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.openmp_switch_combobox = ttk.Combobox(frame)
        self.openmp_switch_combobox['values'] = ["-qopenmp", "-fopenmp"]
        self.openmp_switch_combobox.set(["Yes" if self.settings.open_mp_switch not in [None, False, ''] else "No"])
        self.openmp_switch_combobox.grid(column=1, row=3, padx=10, pady=5, sticky=(tk.W, tk.E))

        # TABS FRAME
        tab_frame = ttk.Frame(labelframe)
        tab_frame.pack(fill=tk.BOTH, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        sys_lib_tab = ttk.Frame(tab_control)
        cus_lib_tab = ttk.Frame(tab_control)
        tab_control.add(sys_lib_tab, text="pkg-config defined:")
        tab_control.add(cus_lib_tab, text="Path defined:")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.NW, pady=5)

        self.system_libraries_pane = SystemLibrariesPane(sys_lib_tab)
        self.custom_libraries_pane = CustomLibrariesPane(cus_lib_tab)

    @staticmethod
    def handle_focus(event):
        """Handle focus event and set focus to the next widget.

        Args:
            event: The focus event.
        """
        event.widget.tk_focusNext().focus()

    def open_filedialog(self):
        """Open the filedialog when the browse button is clicked and change the module path value to selected path.
        """
        filename = tk.filedialog.askopenfilename()
        if filename not in ['', ()]:
            self.module_path.set(filename)

    def reload(self):
        """Reload system settings, set current value of compiler, module path, MPI and OpenMP switch.
        Call SystemLibrariesPane and CustomLibrariesPane reload methods.
        """
        self.compiler_cmd.set(self.settings.compiler_cmd)
        self.module_path.set(self.settings.include_path or "")
        self.mpi_combobox.set([self.settings.mpi or None])
        self.openmp_switch_combobox.set([self.settings.open_mp_switch or None])

        self.system_libraries_pane.reload()
        self.custom_libraries_pane.reload()

    def update_settings(self):
        """Update compiler, module path, MPI and OpenMP switch values in the ProjectSettings. Call SystemLibrariesPane
        and CustomLibrariesPane update_settings methods.
        """
        ProjectSettings.get_settings().code_description.language_specific.compiler_cmd = self.compiler_cmd.get()
        ProjectSettings.get_settings().code_description.language_specific.include_path = self.module_path.get()
        ProjectSettings.get_settings().code_description.language_specific.mpi = self.mpi_combobox.get()
        ProjectSettings.get_settings().code_description.language_specific.open_mp_switch = self.openmp_switch_combobox.get()

        self.system_libraries_pane.update_settings()
        self.custom_libraries_pane.update_settings()
        self.settings = ProjectSettings.get_settings().code_description.language_specific

    def save_pane_settings(self):
        """Save the data from a language pane to the dictionary using the LanguageSettingsManager.
        """
        # compiler = self.compiler_combobox.get()
        system_libraries = self.system_libraries_pane.get_data_from_table()
        custom_libraries = self.custom_libraries_pane.get_list_of_custom_libraries()
        mpi = self.feature_pane.mpi_flavour_combobox.get()
        open_mpi = True if self.feature_pane.open_mp_combobox.get() == 'Yes' else False
        include_path = self.feature_pane.module_path.get()
        extra_lib = ExtraLibraries()
        extra_lib.pkg_config_defined = system_libraries
        extra_lib.lib_path = custom_libraries
        self.settings.from_dict({'compiler_cmd': compiler,
                                 'include_path': include_path,
                                 '_mpi': mpi,
                                 'open_mp_switch': open_mpi,
                                 'extra_libraries': extra_lib.to_dict()})


class SystemLibrariesPane:
    """The SystemLibrariesPane contains the Table widget with system libraries. Add button opens new window and enables
    to add system library from list of available system libraries. Remove button enables to delete selected library from the Table.

    Attributes:
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        columns (list[Column]): The list of the Column class objects.
        table (Table): The table widget.
        master (ttk.Frame): The master frame.
        system_lib (PkgConfigTools): The PkgConfigTools class object.

    """
    def __init__(self, master=None):
        """Initialize the SystemLibrariesPane class object.

        Args:
            master (ttk.Frame): The master frame.
        """
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)
        self.system_lib = PkgConfigTools()
        self.system_lib.initialize()
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
        remove_button = ttk.Button(buttons_center_frame, text="Remove", width=10)
        remove_button.pack(side=tk.TOP, expand=1, pady=5)

        # TABLE
        self.columns = [Column(Column.TEXT, "Name", "Name"),
                        Column(Column.TEXT, "Info", "Info"),
                        Column(Column.TEXT, "Description", "Description")]
        self.table = Table([], self.columns, table_frame, [remove_button])
        add_button['command'] = lambda: AddSystemLibraryWindow(self)
        remove_button['command'] = self.table.delete_row
        self.__add_table_data()

    def __add_table_data(self):
        """Add system libraries to the table.
        """
        self.table.delete_data_from_table()
        if not self.settings.extra_libraries.pkg_config_defined:
            return

        data = []
        for sys_lib in self.settings.extra_libraries.pkg_config_defined:
            name = sys_lib
            system_lib_dict = self.system_lib.get_pkg_config(name)
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
        """Get system libraries names from the table.

        Returns (list): The list with system libraries names from the table.
        """
        system_libraries = self.table.get_data_from_table()
        libraries_name = []
        for system_library in system_libraries:
            libraries_name.append(system_library['Name'])

        return libraries_name

    def reload(self):
        """Reload system settings from the LanguageSettingsManager and add system libraries to the Table widget.
        """
        self.settings = ProjectSettings.get_settings().code_description.language_specific
        self.__add_table_data()

    def update_settings(self):
        """Update system libraries in the ProjectSettings.
        """
        system_libraries = self.get_data_from_table()
        ProjectSettings.get_settings().code_description.language_specific.extra_libraries.pkg_config_defined = system_libraries


class AddSystemLibraryWindow:
    """Opens a new window with a table contains available system libraries. The table can be filtered by user.

    Attributes:
        master (SystemLibrariesPane): The master pane.
        window (tk.Toplevel): The new window with system libraries table.
        table (Table): The table contains system libraries.
    """
    def __init__(self, master=None):
        """Initialize the AddSystemLibraryWindow class object.

        Args:
            master (SystemLibrariesPane): The master pane.
        """
        self.master = master

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

        # TABLE
        system_lib_dict = master.system_lib.system_lib_dict
        data = []
        for key, value in system_lib_dict.items():
            data.append([key, value['info'], value['description']])
        self.table = Table([], master.columns, content_frame)
        self.table.add_rows(data)

        # BUTTONS
        add_button = ttk.Button(footer, text="OK", command=self.add_selected_data_to_table, width=10)
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


class CustomLibrariesPane:
    """The CustomLibrariesPane contains the Table widget with the custom libraries. Add button enables to add library
    path from filedialog to the Table, remove button enables to delete selected library from the Table.

    Attributes:
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        table (Table): The Table contains custom libraries.
    """
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
        self.__add_custom_lib_from_settings()

    def __add_custom_lib_from_settings(self):
        """Add custom libraries from the ProjectSettings to the Table widget.
        """
        data = []
        if self.settings.extra_libraries.lib_path is not None:
            for cus_lib in self.settings.extra_libraries.lib_path:
                data.append([cus_lib])
        self.table.add_new_table_content(data)

    def __add_on_click(self):
        """Open the filedialog and add selected path to the Table widget.
        """
        path = tk.filedialog.askopenfilename()
        if path not in ['', ()]:
            self.table.add_rows([[path]])

    def get_list_of_custom_libraries(self):
        """Get the list of custom libraries from the table.

        Returns: The list of custom libraries.
        """
        data_table = self.table.get_data_from_table()
        return [data['Library path'] for data in data_table]

    def reload(self):
        """Reload custom_libraries list from the LanguageSettingsManager and add it to the Table widget.
        """
        self.settings = ProjectSettings.get_settings().code_description.language_specific
        self.__add_custom_lib_from_settings()

    def update_settings(self):
        """Update custom_libraries in the ProjectSettings.
        """
        custom_libraries = self.get_list_of_custom_libraries()
        ProjectSettings.get_settings().code_description.language_specific.extra_libraries.lib_path = custom_libraries
