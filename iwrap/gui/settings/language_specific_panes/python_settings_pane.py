import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class PythonPane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )
        ttk.Label( self, text="Place for SPECIFIC Python settings", borderwidth=1, relief="solid" ).pack( fill=tk.BOTH, expand=1 )

    def reload(self):
        pass