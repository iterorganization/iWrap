import logging
import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class CppPane( ttk.Frame, IWrapPane ):
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        super().__init__( master )
        ttk.Label( self, text="Place for SPECIFIC CPP settings", borderwidth=1, relief="solid" ).pack( fill=tk.BOTH, expand=1 )

    def update_settings(self):
        pass

    def reload(self):
        pass

    def save_pane_settings(self):
        pass