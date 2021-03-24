import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.gui.settings.language_specific_panes.language_panes_mgmt import LanguagePanesManager


class CodeSettingsPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

        language_pane = LanguagePanesManager.get_language_pane('Python')
        pane = language_pane(self)
        pane.pack( fill=tk.BOTH, expand=1 )

    def reload(self):
        pass
