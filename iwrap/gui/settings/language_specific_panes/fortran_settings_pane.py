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
        self.tab_control = ttk.Notebook(tab_frame)
        self.feature_tab = ttk.Frame(self.tab_control)
        self.sys_lib_tab = ttk.Frame(self.tab_control)
        self.cus_lib_tab = ttk.Frame(self.tab_control)
        self.tab_control.add(self.feature_tab, text="Features")
        self.tab_control.add(self.sys_lib_tab, text="System libraries")
        self.tab_control.add(self.cus_lib_tab, text="Custom libraries")
        self.tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.NW)

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
