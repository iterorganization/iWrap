import tkinter as tk
from tkinter import ttk
import tkinter.filedialog

from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table
from iwrap.gui.widgets.table import Column
from iwrap.settings.language_specific.fortran_settings import FortranSpecificSettings
from iwrap.settings.project import ProjectSettings


class FortranPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        self.settings = FortranSpecificSettings()
        self.combobox_values = ['Intel Fortran (ifort)', 'GNU Compiler Collection (fortran)', 'Intel']

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
        self.compiler_combobox['values'] = self.combobox_values
        self.compiler_combobox.current(0)
        self.compiler_combobox.bind("<<ComboboxSelected>>", self.update_compiler)
        self.compiler_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # TABS FRAME
        tab_frame = ttk.Frame(labelframe)
        tab_frame.pack(fill=tk.BOTH, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        self.sys_lib_tab = ttk.Frame(tab_control)
        self.cus_lib_tab = ttk.Frame(tab_control)
        self.feature_lib_tab = ttk.Frame(tab_control)
        tab_control.add(self.feature_lib_tab, text="Features")
        tab_control.add(self.sys_lib_tab, text="System libraries")
        tab_control.add(self.cus_lib_tab, text="Custom libraries")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.NW, pady=5)

        self.feature_pane = FeaturesPane(self)
        self.system_libraries_pane = SystemLibrariesPane(self)
        self.custom_libraries_pane = CustomLibrariesPane(self)

    def update_compiler(self, eventObject=None):
        self.settings.compiler = self.compiler_combobox.get()

    def reload(self):
        dict_settings = ProjectSettings.get_settings().code_description.language_specific
        if dict_settings is None:
            self.settings.clear()
        else:
            self.settings.from_dict(dict_settings)

        self.compiler_combobox.set(self.settings.compiler)

        self.feature_pane.reload()
        self.system_libraries_pane.reload()
        self.custom_libraries_pane.reload()

    def update_settings(self):
        dict_settings = self.settings.to_dict()
        ProjectSettings.get_settings().code_description.language_specific = dict_settings


class SystemLibrariesPane:
    def __init__(self, master=None):
        self.settings = master.settings
        master_frame = master.sys_lib_tab

        # TREE VIEW FRAME
        self.tree_view_frame = ttk.Frame(master_frame)
        self.tree_view_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, anchor=tk.NW)

        # BUTTONS FRAMES
        buttons_frame = ttk.Frame(master_frame, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, anchor=tk.NE)
        buttons_center_frame = ttk.Frame(buttons_frame)
        buttons_center_frame.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # TABLE
        self.columns = [Column(Column.TEXT, "Name", "Name"),
                        Column(Column.TEXT, "Info", "Info"),
                        Column(Column.TEXT, "Description", "Description")]

        self.table = Table([], self.columns, self.tree_view_frame)

        # BUTTONS
        ttk.Button(buttons_center_frame, text="Add", command=lambda: self.table.add_row("system library"), width=10)\
            .pack(side=tk.TOP, expand=1, pady=5)
        # ttk.Button(buttons_center_frame, text="Info", width=10).pack(side=tk.TOP, expand=1, pady=5)
        ttk.Button(buttons_center_frame, text="Remove", command=self.table.delete_row, width=10)\
            .pack(side=tk.TOP, expand=1, pady=5)

    def add_table_data(self):
        data = []
        for sys_lib in self.settings.system_libraries:
            data.append([sys_lib, 'example_info', 'example_description'])

        self.table.add_new_table(data, self.columns)

    def get_data_from_table(self):
        system_libraries = self.table.get_data_from_table()
        libraries_name = []
        for system_library in system_libraries:
            libraries_name.append(system_library['Name'])

        return libraries_name

    def reload(self):
        dict_settings = ProjectSettings.get_settings().code_description.language_specific
        if dict_settings is None:
            self.settings.clear()
        else:
            self.settings.from_dict(dict_settings)

        self.add_table_data()

    def update_settings(self):
        libraries_name = self.get_data_from_table()
        ProjectSettings.get_settings().code_description.language_specific['system_libraries'] = libraries_name


class CustomLibrariesPane:
    def __init__(self, master=None):
        self.settings = master.settings
        master_frame = master.cus_lib_tab

        # LIBRARY PATH FRAME
        library_path_frame = tk.Frame(master_frame)
        library_path_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, anchor=tk.NW)

        # BUTTONS FRAMES
        buttons_frame = ttk.Frame(master_frame, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, anchor=tk.NE)
        buttons_center_frame = ttk.Frame(buttons_frame)
        buttons_center_frame.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # BUTTONS
        ttk.Button(buttons_center_frame, text="Add...", command=self.add_on_click, width=10)\
            .pack(side=tk.TOP, expand=1, pady=5)
        ttk.Button(buttons_center_frame, text="Remove", command=self.remove_on_click, width=10)\
            .pack(side=tk.TOP, expand=1, pady=5)

        # LIBRARY PATH LABEL
        labelframe = ttk.Frame(library_path_frame, height=20)
        labelframe.pack(fill=tk.X, side=tk.TOP, expand=0, anchor=tk.NW)
        ttk.Label(labelframe, text="Library path:", border=None, relief="flat").pack(side=tk.TOP, expand=1, pady=5)

        # LISTBOX WITH SCROLLBAR
        scrollbar = tk.Scrollbar(library_path_frame, orient=tk.VERTICAL)
        self.listbox = tk.Listbox(library_path_frame, yscrollcommand=scrollbar.set, selectmode=tk.MULTIPLE)
        self.listbox.pack(side=tk.TOP, fill=tk.BOTH, anchor='nw', expand=1)

    def add_custom_lib_from_settings(self):
        for cus_lib in self.settings.custom_libraries:
            self.listbox.insert(tk.END, cus_lib)

    def add_on_click(self):
        path = tk.filedialog.askopenfilename()
        self.listbox.insert(tk.END, path)
        self.settings.custom_libraries.append(path)

    def remove_on_click(self):
        selected_paths = self.listbox.curselection()
        for index in selected_paths[::-1]:
            self.listbox.delete(index)
            self.settings.custom_libraries.pop(index)

    def reload(self):
        dict_settings = ProjectSettings.get_settings().code_description.language_specific
        if dict_settings is None:
            self.settings.clear()
        else:
            self.settings.from_dict(dict_settings)

        self.add_custom_lib_from_settings()

    def update_settings(self):
        dict_settings = self.settings.to_dict()
        ProjectSettings.get_settings().code_description.language_specific = dict_settings


class FeaturesPane:
    def __init__(self, master=None):
        self.settings = master.settings
        master_frame = master.feature_lib_tab

        # LABEL FRAME
        labelframe = ttk.LabelFrame(master_frame, text="Computation", borderwidth=2, relief="groove")
        labelframe.pack(side=tk.LEFT, fill=tk.BOTH, expand=1, pady=5)

        # COMBOBOX MPI Flavour
        ttk.Label(labelframe, text="MPI Flavour:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.mpi_flavour_combobox = ttk.Combobox(labelframe, state='readonly')
        self.mpi_flavour_combobox['values'] = ["MPICH", "OpenMPI"]
        self.mpi_flavour_combobox.current(0)
        self.mpi_flavour_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # COMBOBOX OpenMPI
        ttk.Label(labelframe, text="OpenMPI:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.open_mpi_combobox = ttk.Combobox(labelframe, state='readonly')
        self.open_mpi_combobox['values'] = ["Yes", "No"]
        self.open_mpi_combobox.current(0)
        self.open_mpi_combobox.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))

    def reload(self):
        self.settings = FortranSpecificSettings()
        self.mpi_flavour_combobox.set(self.settings.mpi)
        self.open_mpi_combobox.set(["Yes" if self.settings.open_mp else "No"])

    def update_settings(self):
        pass
