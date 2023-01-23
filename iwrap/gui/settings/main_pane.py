import logging
import tkinter as tk
from tkinter import ttk


from iwrap.gui.generics import IWrapPane
from iwrap.gui.settings.arguments_pane import ArgumentsPane
from iwrap.gui.settings.documentation_pane import DocumentationPane
from iwrap.gui.settings.signature_pane import SignaturePane
from iwrap.gui.settings.implementation_pane import ImplementationPane
from iwrap.gui.settings.settings_pane import SettingsPane

from iwrap.settings.project import ProjectSettings


class SettingsMainPane( ttk.LabelFrame, IWrapPane ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        super().__init__( master, text="Code description:", relief="groove", borderwidth=2, height=100 )

        self.pack( pady=10 )
        self.notebook = ttk.Notebook( self )
        self.notebook.pack( expand=True, fill=tk.BOTH)

        self.arguments_pane = ArgumentsPane( self )
        self.implementation_pane = ImplementationPane( self )
        self.settings_pane = SettingsPane( self )
        self.documentation_pane = DocumentationPane( self )
        self.signature_pane = SignaturePane( self )


        self.notebook.add( self.arguments_pane, text='Arguments' )
        self.notebook.add(self.implementation_pane, text="Implementation")
        self.notebook.add( self.settings_pane, text="Settings" )
        self.notebook.add( self.documentation_pane, text='Documentation' )


        # Set tab index property with notebook's index
        #self.signature_pane.tab_index = notebook.index(self.signature_pane)

        self.notebook.select( None )
        self.notebook.enable_traversal()
        # When tab is changed execute event handler.
        self.notebook.bind("<<NotebookTabChanged>>", self.event_handler)
        self.implementation_pane.programming_language_combobox.bind("<<ComboboxSelected>>", self.change_language)

    def event_handler(self, event) -> None:
        """Conditions events."""
        if event.widget.index("current") == self.signature_pane.tab_index:
            self.signature_pane.reload()

    def change_language(self, event) -> None:
        """Update specific language pane when programming language in combobox is changed.

        Args:
            event: Combobox change value event object. Default to None.
        """
        selected_language = self.implementation_pane.programming_language_combobox.get()
        self.settings_pane.change_language( selected_language )
        self.implementation_pane.selected_programming_language.set( selected_language )

    def update_settings(self):
        self.arguments_pane.update_settings()
        self.implementation_pane.update_settings()
        self.documentation_pane.update_settings()
        self.settings_pane.update_settings()

    def reload(self):
        self.arguments_pane.reload()
        self.settings_pane.reload()
        self.implementation_pane.reload()
        self.documentation_pane.reload()

