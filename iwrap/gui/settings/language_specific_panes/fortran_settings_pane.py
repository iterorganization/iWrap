import tkinter as tk
from tkinter import ttk
import tkinter.filedialog
from tkinter import messagebox

from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table
from iwrap.gui.widgets.table import Column
from iwrap.settings.language_specific.language_settings_mgmt import LanguageSettingsManager
from iwrap.settings.platform.pkg_config_tools import PkgConfigTools

from iwrap.settings.project import ProjectSettings


class FortranPane( ttk.Frame, IWrapPane ):
    """The FortranPane contains a combobox for selecting compiler and three tabs including feature frame, system library
    frame, and custom library frame. Feature frame contains two Combobox widgets and enables the selection of MPI
    Flavour and OpenMPI, system library frame contains the Table widget with system libraries and the custom library
    frame contains the ListBox widget with custom libraries.

    Attributes:
        language (string): The language related to the class.
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        compiler_combobox (ttk.Combobox): The compiler combobox.
        feature_pane (FeaturesPane): The FeaturesPane class object.
        system_libraries_pane (SystemLibrariesPane): The SystemLibrariesPane class object.
        custom_libraries_pane (CustomLibrariesPane): The CustomLibrariesPane class object.
    """
    language = 'Fortran'

    def __init__(self, master=None):
        """Initialize the FortranPane class object.

        Args:
            master (ttk.Frame): The master frame.
        """
        super().__init__( master )
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Language specific settings", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, expand=1)

        # COMBOBOX FRAME
        combobox_frame = ttk.Frame(labelframe)
        combobox_frame.pack(fill=tk.BOTH, side=tk.TOP, expand=0, anchor=tk.NW)
        combobox_frame.grid_columnconfigure(1, weight=1)

        # COMBOBOX
        ttk.Label(combobox_frame, text="Compiler:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.compiler_combobox = ttk.Combobox(combobox_frame, state='readonly')
        self.compiler_combobox['values'] = ['Intel Fortran (ifort)', 'GNU Compiler Collection (fortran)', 'Intel']
        self.compiler_combobox.set(self.settings.compiler)
        self.compiler_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # TABS FRAME
        tab_frame = ttk.Frame(labelframe)
        tab_frame.pack(fill=tk.BOTH, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        sys_lib_tab = ttk.Frame(tab_control)
        cus_lib_tab = ttk.Frame(tab_control)
        feature_lib_tab = ttk.Frame(tab_control)
        tab_control.add(feature_lib_tab, text="Features")
        tab_control.add(sys_lib_tab, text="System libraries")
        tab_control.add(cus_lib_tab, text="Custom libraries")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.NW, pady=5)

        self.feature_pane = FeaturesPane(feature_lib_tab)
        self.system_libraries_pane = SystemLibrariesPane(sys_lib_tab)
        self.custom_libraries_pane = CustomLibrariesPane(cus_lib_tab)

    def reload(self):
        """Reload system settings from the LanguageSettingsManager, set compiler to the Combobox widget as current value.
        Call SystemLibrariesPane, CustomLibrariesPane, and FeaturesPane reload methods.
        """
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)

        self.compiler_combobox.set(self.settings.compiler)
        self.feature_pane.reload()
        self.system_libraries_pane.reload()
        self.custom_libraries_pane.reload()

    def update_settings(self):
        """Update compiler value in the ProjectSettings. Call SystemLibrariesPane, CustomLibrariesPane and
         FeaturesPane update_settings methods.
        """
        compiler = self.compiler_combobox.get()
        ProjectSettings.get_settings().code_description.language_specific['compiler'] = compiler

        self.system_libraries_pane.update_settings()
        self.custom_libraries_pane.update_settings()
        self.feature_pane.update_settings()

        project_settings = ProjectSettings.get_settings().code_description.language_specific
        self.settings.from_dict(project_settings)

    def save_pane_settings(self):
        """Save the data from a language pane to the dictionary using the LanguageSettingsManager.
        """
        compiler = self.compiler_combobox.get()
        system_libraries = self.system_libraries_pane.get_data_from_table()
        custom_libraries = list(self.custom_libraries_pane.listbox.get(0, tk.END))
        mpi = self.feature_pane.mpi_flavour_combobox.get()
        open_mpi = True if self.feature_pane.open_mpi_combobox.get() == 'Yes' else False
        self.settings.from_dict({'compiler': compiler,
                                 'mpi': mpi,
                                 'open_mp': open_mpi,
                                 'system_libraries': system_libraries,
                                 'custom_libraries': custom_libraries})


class SystemLibrariesPane:
    """The SystemLibrariesPane contains the Table widget with system libraries. Add button enables to add system library
    to the Table, remove button enables to delete selected library from the Table.

    Attributes:
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        columns (list[Column]): The list of the Column class objects.
        table (Table): The table widget.
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

        # TABLE
        self.columns = [Column(Column.TEXT, "Name", "Name"),
                        Column(Column.TEXT, "Info", "Info"),
                        Column(Column.TEXT, "Description", "Description")]

        # BUTTONS
        add_button = ttk.Button(buttons_center_frame, text="Add...", width=10)
        add_button.pack(side=tk.TOP, expand=1, pady=5)
        remove_button = ttk.Button(buttons_center_frame, text="Remove", width=10)
        remove_button.pack(side=tk.TOP, expand=1, pady=5)

        self.table = Table([], self.columns, table_frame, [remove_button])
        add_button['command'] = self.initialize_add_system_library_pane
        remove_button['command'] = self.table.delete_row
        self.__add_table_data()

    def initialize_add_system_library_pane(self):
        pass

    def __add_table_data(self):
        """Add system libraries to the table.
        """
        data = []
        for sys_lib in self.settings.system_libraries:
            name = sys_lib
            system_lib_dict = self.system_lib.get_pkg_config(name)
            if system_lib_dict is None:
                messagebox.showwarning("Warning", f"Unknown system library.")
            else:
                info = system_lib_dict['info']
                description = system_lib_dict['description']
                data.append([name, info, description])

        self.table.add_new_table(data, self.columns)

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
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)
        self.__add_table_data()

    def update_settings(self):
        """Update system libraries in the ProjectSettings.
        """
        libraries_name = self.get_data_from_table()
        ProjectSettings.get_settings().code_description.language_specific['system_libraries'] = libraries_name


class CustomLibrariesPane:
    """The CustomLibrariesPane contains the ListBox widget with the custom libraries. Add button enables to add library
    path from filedialog to the ListBox, remove button enables to delete selected library from the ListBox.

    Attributes:
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        listbox (tk.Listbox): The listbox contains custom libraries.
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
        ttk.Button(buttons_center_frame, text="Remove", command=self.__remove_on_click, width=10)\
            .pack(side=tk.TOP, expand=1, pady=5)

        # LIBRARY PATH LABEL
        labelframe = ttk.Frame(library_path_frame, height=20)
        labelframe.pack(fill=tk.X, side=tk.TOP, expand=0, anchor=tk.NW)
        ttk.Label(labelframe, text="Library path:", border=None, relief="flat").pack(side=tk.TOP, expand=1, pady=5)

        # LISTBOX WITH SCROLLBAR
        scrollbar = tk.Scrollbar(library_path_frame, orient=tk.VERTICAL)
        self.listbox = tk.Listbox(library_path_frame, yscrollcommand=scrollbar.set, selectmode=tk.MULTIPLE)
        self.listbox.pack(side=tk.TOP, fill=tk.BOTH, anchor='nw', expand=1)

        self.__add_custom_lib_from_settings()

    def __add_custom_lib_from_settings(self):
        """Add custom libraries from the ProjectSettings to the ListBox widget.
        """
        self.listbox.delete(0, tk.END)
        for cus_lib in self.settings.custom_libraries:
            self.listbox.insert(tk.END, cus_lib)

    def __add_on_click(self):
        """Open the filedialog and add selected path to the ListBox widget.
        """
        path = tk.filedialog.askopenfilename()
        self.listbox.insert(tk.END, path)
        self.settings.custom_libraries.append(path)

    def __remove_on_click(self):
        """Remove clicked custom library from the ListBox widget.
        """
        selected_paths = self.listbox.curselection()
        for index in selected_paths[::-1]:
            self.listbox.delete(index)
            self.settings.custom_libraries.pop(index)

    def reload(self):
        """Reload custom_libraries list from the LanguageSettingsManager and add it to the ListBox widget.
        """
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)
        self.__add_custom_lib_from_settings()

    def update_settings(self):
        """Update custom_libraries in the ProjectSettings.
        """
        custom_libraries = list(self.listbox.get(0, tk.END))
        ProjectSettings.get_settings().code_description.language_specific['custom_libraries'] = custom_libraries


class FeaturesPane:
    """The FeaturesPane contains two Combobox widgets and enables the selection of MPI Flavour and OpenMPI.

    Attributes:
        settings (LanguageSettingsManager): The project settings for fortran language pane.
        mpi_flavour_combobox (ttk.Combobox): The combobox contains mpi flavour values.
        open_mpi_combobox (ttk.Combobox): The combobox contains open mpi values.
    """

    def __init__(self, master=None):
        """Initialize the FeaturesPane class object.

        Args:
            master (ttk.Frame): The master frame.
        """
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)

        # LABEL FRAME
        labelframe = ttk.LabelFrame(master, text="Computation", borderwidth=2, relief="groove")
        labelframe.pack(side=tk.LEFT, fill=tk.BOTH, expand=1, pady=5)

        # COMBOBOX MPI Flavour
        ttk.Label(labelframe, text="MPI Flavour:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.mpi_flavour_combobox = ttk.Combobox(labelframe, state='readonly')
        self.mpi_flavour_combobox['values'] = ["MPICH2", "OpenMPI"]
        if self.settings.mpi != '':
            self.mpi_flavour_combobox.set(self.settings.mpi)
        else:
            self.mpi_flavour_combobox.current(0)
        self.mpi_flavour_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # COMBOBOX OpenMPI
        ttk.Label(labelframe, text="OpenMPI:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.open_mpi_combobox = ttk.Combobox(labelframe, state='readonly')
        self.open_mpi_combobox['values'] = ["Yes", "No"]
        self.open_mpi_combobox.set(["Yes" if self.settings.open_mp else "No"])
        self.open_mpi_combobox.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))

    def reload(self):
        """Reload open_mpi and mpi values from the LanguageSettingsManager and set them to the Combobox widgets.
        """
        self.settings = LanguageSettingsManager.get_settings(FortranPane.language)
        if self.settings.mpi != '':
            self.mpi_flavour_combobox.set(self.settings.mpi)
        else:
            self.mpi_flavour_combobox.current(0)
        self.open_mpi_combobox.set(["Yes" if self.settings.open_mp else "No"])

    def update_settings(self):
        """Update open_mpi and mpi values in the ProjectSettings.
        """
        ProjectSettings.get_settings().code_description.language_specific['mpi'] = self.mpi_flavour_combobox.get()
        open_mpi = True if self.open_mpi_combobox.get() == 'Yes' else False
        ProjectSettings.get_settings().code_description.language_specific['open_mp'] = open_mpi
