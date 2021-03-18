import tkinter as tk
from tkinter import ttk


class DocumentationPane( ttk.Frame ):
    def __init__(self, master=None):
        super().__init__( master )
        ttk.Label( self, text='Place for DOCUMENTATION', borderwidth=1, relief="solid" ).pack(fill=tk.BOTH, expand=1)