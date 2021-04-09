import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
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
        notebook.pack( expand=True, fill=tk.BOTH )

        self.arguments_pane = ArgumentsPane( notebook )
        self.code_settings_pane = CodeSettingsPane( notebook )
        self.code_parameters_pane = CodeParametersPane( notebook )
        self.documentation_pane = DocumentationPane( notebook )
        self.signature_pane = SignaturePane( notebook )

        notebook.add( self.arguments_pane, text='Arguments' )
        notebook.add( self.code_settings_pane, text='Code settings' )
        notebook.add( self.code_parameters_pane, text='Code parameters' )
        notebook.add( self.documentation_pane, text='Documentation' )
        notebook.add( self.signature_pane, text='Signature' )

        notebook.select( None )
        notebook.enable_traversal()

    def update_settings(self):
        self.arguments_pane.update_settings()
        self.code_settings_pane.update_settings()
        self.code_parameters_pane.update_settings()
        self.documentation_pane.update_settings()
        self.signature_pane.update_settings()

    def reload(self):
        self.arguments_pane.reload()
        self.code_settings_pane.reload()
        self.code_parameters_pane.reload()
        self.documentation_pane.reload()
        self.signature_pane.reload()
