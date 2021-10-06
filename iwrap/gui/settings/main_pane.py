import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.gui.settings.language_specific_panes.language_panes_mgmt import LanguagePanesManager
from iwrap.settings.language_specific.language_settings_mgmt import LanguageSettingsManager
from iwrap.gui.settings.arguments_pane import ArgumentsPane
from iwrap.gui.settings.code_parameters_pane import CodeParametersPane
from iwrap.gui.settings.code_settings_pane import CodeSettingsPane
from iwrap.gui.settings.documentation_pane import DocumentationPane
from iwrap.gui.settings.signature_pane import SignaturePane


class SettingsMainPane( ttk.LabelFrame, IWrapPane ):

    def __init__(self, master=None):
        super().__init__( master, text="Wrapped code description:", relief="groove", borderwidth=2, height=100 )

        self.pack( pady=10 )
        notebook = ttk.Notebook( self )
        notebook.pack( expand=True, fill=tk.BOTH)

        self.arguments_pane = ArgumentsPane( notebook )
        self.code_settings_pane = CodeSettingsPane( notebook )
        self.code_parameters_pane = CodeParametersPane( notebook )
        self.documentation_pane = DocumentationPane( notebook )
        self.signature_pane = SignaturePane( notebook )

        self.language_settings_pane = None
        self.add_language_pane(notebook)
        self.code_settings_pane.programming_language_combobox.bind("<<ComboboxSelected>>", self.change_language_pane)

        notebook.add( self.arguments_pane, text='Arguments' )
        notebook.add( self.code_settings_pane, text='Code settings' )
        notebook.add( self.language_settings_pane, text='Language settings' )
        notebook.add( self.code_parameters_pane, text='Code parameters' )
        notebook.add( self.documentation_pane, text='Documentation' )
        #notebook.add( self.signature_pane, text='Signature' )

        # Set tab index property with notebook's index
        #self.signature_pane.tab_index = notebook.index(self.signature_pane)

        notebook.select( None )
        notebook.enable_traversal()
        # When tab is changed execute event handler.
        notebook.bind("<<NotebookTabChanged>>", self.event_handler)

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
            self.save_pane_settings.pack_forget()
            self.add_language_pane()

    def add_language_pane(self, notebook):
        """Add specific language pane for selected programming language.
        """
        selected_language = self.code_settings_pane.selected_programming_language.get()
        language_pane_manager = LanguagePanesManager.get_language_pane(selected_language)
        self.language_settings_pane = language_pane_manager(notebook, selected_language)

    def update_settings(self):
        self.arguments_pane.update_settings()
        self.code_settings_pane.update_settings()
        self.language_settings_pane.update_settings()
        self.code_parameters_pane.update_settings()
        self.documentation_pane.update_settings()
        self.signature_pane.update_settings()

    def reload(self):
        self.arguments_pane.reload()
        self.code_settings_pane.reload()
        self.language_settings_pane.reload()
        self.code_parameters_pane.reload()
        self.documentation_pane.reload()
        self.signature_pane.reload()
