import tkinter as tk
from tkinter import filedialog

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings
from iwrap.settings.serialization import YAMLSerializer


class MenuBar( tk.Menu ):

    def __init__(self, master: IWrapPane):
        super().__init__( master )
        self.main_window = master
        self.code_description = ProjectSettings.get_settings().code_description

        file_menu = tk.Menu( self, tearoff=0 )
        file_menu.add_command( label='New', command=self.action_new )
        file_menu.add_separator()

        import_menu = tk.Menu( file_menu, tearoff=0 )
        import_menu.add_command( label='Import code description...', command=self.action_import )
        file_menu.add_cascade( label='Import', menu=import_menu )

        export_menu = tk.Menu( file_menu, tearoff=0 )
        export_menu.add_command( label='Export code description...', command=self.action_export )
        file_menu.add_cascade( label='Export', menu=export_menu )

        file_menu.add_separator()
        file_menu.add_command( label='Quit', command=master.winfo_toplevel().destroy )

        self.add_cascade( label='File', menu=file_menu )

    def action_new(self):
        ProjectSettings.get_settings().clear()
        self.main_window.reload()

    def action_export(self):
        file = filedialog.asksaveasfile( initialdir=None,
                                       title=None,
                                       filetypes=(("YAML files",
                                                   "*.yaml"),) )
        if file is None:
            return

        self.code_description.save( YAMLSerializer( file ) )
        file.close()

    def action_import(self):
        file = filedialog.askopenfile( initialdir=None,
                                         title=None,
                                         filetypes=(("YAML files",
                                                     "*.yaml"),),
                                         defaultextension='yaml')

        if file is None:
            return

        # Loading project settings from file
        self.code_description.load( YAMLSerializer( file ) )
        file.close()
        self.main_window.reload()
