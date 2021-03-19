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

        notebook.add( ArgumentsPane( notebook ), text='Arguments' )
        notebook.add( CodeSettingsPane( notebook ), text='Code settings' )
        notebook.add( CodeParametersPane( notebook ), text='Code parameters' )
        notebook.add( DocumentationPane( notebook ), text='Documentation' )
        notebook.add( SignaturePane( notebook ), text='Signature' )

        notebook.select( None )
        notebook.enable_traversal()

    def reload(self):
        pass
