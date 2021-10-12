import logging
import os
import tkinter as tk
from tkinter import filedialog

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class MenuBar( tk.Menu ):
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self, master: IWrapPane):
        super().__init__( master )
        self.main_window = master
        self.code_description = ProjectSettings.get_settings().code_description
        self.__save_and_open_initialdir = ProjectSettings.get_settings().root_dir
        self.__import_and_export_initialdir = ProjectSettings.get_settings().root_dir

        file_menu = tk.Menu( self, tearoff=0 )
        file_menu.add_command( label='New', command=self.action_new )
        file_menu.add_command( label='Open...', command=self.action_open )
        file_menu.add_command( label='Save as...', command=self.action_save_as)
        file_menu.add_command( label='Save', command=self.action_save)

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
        file = filedialog.asksaveasfile( initialdir=self.__import_and_export_initialdir,
                                       title=None,
                                       filetypes=(("YAML files",
                                                   "*.yaml"),))
        if file is None:
            return

        self.main_window.update_settings()
        ProjectSettings.get_settings().code_description.save(file)
        self.__import_and_export_initialdir = os.path.dirname(file.name)
        file.close()

    def action_import(self):
        file = filedialog.askopenfile( initialdir=self.__import_and_export_initialdir,
                                         title=None,
                                         filetypes=(("YAML files",
                                                     "*.yaml"),),
                                         defaultextension='yaml')

        if file is None:
            return

        # Loading project settings from file
        ProjectSettings.get_settings().clear()
        ProjectSettings.get_settings().code_description.load(file)
        self.__import_and_export_initialdir = os.path.dirname(file.name)
        file.close()
        self.main_window.reload()

    def action_save_as(self):
        file = filedialog.asksaveasfile( initialdir=self.__save_and_open_initialdir,
                                       title=None,
                                       filetypes=(("YAML files",
                                                   "*.yaml"),) )
        if file is None:
            return

        self.main_window.update_settings()
        ProjectSettings.get_settings().save(file)
        file.close()

        file_real_path = os.path.realpath( file.name )
        ProjectSettings.get_settings().project_file_path = file_real_path
        self.__save_and_open_initialdir = os.path.dirname(file.name)

    def action_save(self):
        filename = ProjectSettings.get_settings().project_file_path
        if filename != '':
            self.main_window.update_settings()
            ProjectSettings.get_settings().save(open(filename, 'w'))
        else:
            self.action_save_as()

    def action_open(self):
        file = filedialog.askopenfile( initialdir=self.__save_and_open_initialdir,
                                         title=None,
                                         filetypes=(("YAML files",
                                                     "*.yaml"),),
                                         defaultextension='yaml')

        if file is None:
            return

        # Loading project settings from file
        ProjectSettings.get_settings().load(file)
        file.close()

        self.main_window.reload()
        file_real_path = os.path.realpath( file.name )
        ProjectSettings.get_settings().root_dir = os.path.dirname( file_real_path )
        ProjectSettings.get_settings().project_file_path = file_real_path
        self.__save_and_open_initialdir = os.path.dirname(file.name)
