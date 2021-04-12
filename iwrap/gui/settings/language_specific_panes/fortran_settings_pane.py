import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.settings.language_specific.fortran_settings import FortranSpecificSettings
from iwrap.settings.project import ProjectSettings


class FortranPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        self.settings = FortranSpecificSettings()
        self.combobox_values = ['compiler1', 'compiler2', 'compiler3']

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
        self.compiler_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # TABS FRAME
        tab_frame = ttk.Frame(labelframe)
        tab_frame.pack(fill=tk.BOTH, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        feature_tab = ttk.Frame(tab_control)
        sys_lib_tab = ttk.Frame(tab_control)
        cus_lib_tab = ttk.Frame(tab_control)
        tab_control.add(feature_tab, text="Features")
        tab_control.add(sys_lib_tab, text="System libraries")
        tab_control.add(cus_lib_tab, text="Custom libraries")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.NW)

        feature_pane = FeaturesPane(feature_tab)
        system_libraries_pane = SystemLibrariesPane(sys_lib_tab)
        custom_libraries_pane = CustomLibrariesPane(cus_lib_tab)

    def update_settings(self):
        pass

    def reload(self):
        dict_settings = ProjectSettings.get_settings().code_description.language_specific
        self.settings.from_dict(dict_settings)
        # TO DO: GUI REFRESH
        pass

    def update_settings(self):
        dict_settings = self.settings.to_dict( )
        ProjectSettings.get_settings().code_description.language_specific = dict_settings


class SystemLibrariesPane( ttk.Frame ):
    def __init__(self, master=None):
        super().__init__( master )

        # TREE VIEW FRAME
        tree_view_frame = ttk.Frame(master)
        tree_view_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, anchor=tk.NW)

        # BUTTONS FRAMES
        buttons_frame = ttk.Frame(master, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, anchor=tk.NE)
        buttons_center_frame = ttk.Frame(buttons_frame)
        buttons_center_frame.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # LABEL
        ttk.Label(tree_view_frame, text="Place for tree view", borderwidth=1, relief="solid").pack(fill=tk.BOTH)

        # BUTTONS
        ttk.Button(buttons_center_frame, text="Add", width=10).pack(side=tk.TOP, expand=1, pady=5)
        ttk.Button(buttons_center_frame, text="Info", width=10).pack(side=tk.TOP, expand=1, pady=5)
        ttk.Button(buttons_center_frame, text="Remove", width=10).pack(side=tk.TOP, expand=1, pady=5)


class CustomLibrariesPane( ttk.Frame ):
    def __init__(self, master=None):
        super().__init__( master )

        ttk.Label(master, text="Place for Custom Libraries Pane", borderwidth=1, relief="solid")\
            .pack(fill=tk.BOTH, expand=1)


class FeaturesPane( ttk.Frame ):
    def __init__(self, master=None):
        super().__init__( master )

        ttk.Label(master, text="Place for Features Pane", borderwidth=1, relief="solid")\
            .pack(fill=tk.BOTH, expand=1)
