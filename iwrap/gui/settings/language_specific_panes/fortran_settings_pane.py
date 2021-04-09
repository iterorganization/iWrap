import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class FortranPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )
        ttk.Label( self, text="Place for SPECIFIC FORTRAN settings", borderwidth=1, relief="solid" ).pack( fill=tk.BOTH, expand=1 )

    def update_settings(self):
        pass

    def reload(self):
        pass
