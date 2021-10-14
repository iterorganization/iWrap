import logging
import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.gui.settings.language_specific_panes.language_panes_mgmt import LanguagePanesManager
from iwrap.settings.language_specific.language_settings_mgmt import LanguageSettingsManager
from iwrap.settings.project import ProjectSettings
from iwrap.gui.settings.arguments_pane import ArgumentsPane
from iwrap.gui.settings.code_parameters_pane import CodeParametersPane
from iwrap.gui.settings.code_settings_pane import CodeSettingsPane
from iwrap.gui.settings.documentation_pane import DocumentationPane
from iwrap.gui.settings.signature_pane import SignaturePane
from iwrap.gui.menu import MenuBar


class SettingsMainPane( ttk.LabelFrame, IWrapPane ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self, master=None):
        super().__init__( master, text="Wrapped code description:", relief="groove", borderwidth=2, height=100 )

        self.pack( pady=10 )

        root_dir_frame = ttk.Frame(self)
        root_dir_frame.pack(fill=tk.X, expand=tk.TRUE)
        root_dir_frame.grid_columnconfigure(1, weight=1)


        self.root_dir = tk.StringVar()
        # BROWSE BUTTON AND ENTRY FOR PATH
        ttk.Label(root_dir_frame, text="Root dir:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.browse_text = ttk.Entry(root_dir_frame, textvariable=self.root_dir)
        self.browse_text.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(root_dir_frame, text="Browse...", command=self.on_click, width=10).grid(column=2, row=2, padx=10, pady=5)
        root_dir_frame.grid_columnconfigure(1, weight=1)

        self.notebook = ttk.Notebook( self )
        self.notebook.pack( expand=True, fill=tk.BOTH)

        self.arguments_pane = ArgumentsPane( self.notebook )
        self.code_settings_pane = CodeSettingsPane( self.notebook )
        self.code_parameters_pane = CodeParametersPane( self.notebook )
        self.documentation_pane = DocumentationPane( self.notebook )
        self.signature_pane = SignaturePane( self.notebook )

        self.language_settings_pane = None
        self.notebook.add( self.arguments_pane, text='Arguments' )
        self.notebook.add( self.code_settings_pane, text='Code settings' )
        self.notebook.add( self.code_parameters_pane, text='Code parameters' )
        self.notebook.add( self.documentation_pane, text='Documentation' )
        self.add_language_pane()
        #notebook.add( self.signature_pane, text='Signature' )

        # Set tab index property with notebook's index
        #self.signature_pane.tab_index = notebook.index(self.signature_pane)

        self.notebook.select( None )
        self.notebook.enable_traversal()
        # When tab is changed execute event handler.
        self.notebook.bind("<<NotebookTabChanged>>", self.event_handler)
        self.code_settings_pane.programming_language_combobox.bind("<<ComboboxSelected>>", self.change_language_pane)

    def event_handler(self, event) -> None:
        """Conditions events."""
        if event.widget.index("current") == self.signature_pane.tab_index:
            self.signature_pane.reload()

    def change_language_pane(self, event) -> None:
        """Update specific language pane when programming language in combobox is changed.

        Args:
            event: Combobox change value event object. Default to None.
        """
        selected_language = self.code_settings_pane.programming_language_combobox.get()
        current_language = self.code_settings_pane.selected_programming_language.get()
        if current_language != selected_language:
            self.code_settings_pane.selected_programming_language.set(selected_language)
            self.language_settings_pane.save_pane_settings()
            self.notebook.forget(self.language_settings_pane)
            self.add_language_pane()

    def add_language_pane(self):
        """Add specific language pane for selected programming language.
        """
        selected_language = self.code_settings_pane.selected_programming_language.get()
        language_pane_manager = LanguagePanesManager.get_language_pane(selected_language)
        self.language_settings_pane = language_pane_manager(self.notebook, selected_language)
        self.notebook.insert(2, self.language_settings_pane, text="Language settings")

    def update_settings(self):
        ProjectSettings.get_settings().code_description.root_dir = self.root_dir.get()
        self.arguments_pane.update_settings()
        self.code_settings_pane.update_settings()
        self.language_settings_pane.update_settings()
        self.code_parameters_pane.update_settings()
        self.documentation_pane.update_settings()
        self.signature_pane.update_settings()

    def reload(self):
        self.root_dir.set(ProjectSettings.get_settings().code_description.root_dir)
        self.arguments_pane.reload()
        self.code_settings_pane.reload()
        self.language_settings_pane.reload()
        self.code_parameters_pane.reload()
        self.documentation_pane.reload()
        self.signature_pane.reload()

    def on_click(self):
        """Open the filedialog when the browse button is clicked and insert selected path to the browse_text entry.
        """
        old_dir = self.root_dir.get()
        dir_name = tk.filedialog.askdirectory()
        if dir_name:
            self.root_dir.set(dir_name)
            if old_dir == MenuBar.save_and_open_initialdir:
                MenuBar.save_and_open_initialdir = dir_name
            if old_dir == MenuBar.import_and_export_initialdir:
                MenuBar.import_and_export_initialdir = dir_name
