import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class SignaturePane( ttk.Frame, IWrapPane ):
    def __init__(self, master=None):
        super().__init__( master )

    def update_settings(self):
        pass

    def reload(self):
        pass


class TextBox(ttk.Frame):
    def __init__(self, master=None) -> None:
        super().__init__(master)