import logging
import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class PythonPane( ttk.Frame, IWrapPane ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None, language=None):
        super().__init__( master )



    def update_settings(self):
        pass

    def reload(self):
        pass

    def save_pane_settings(self):
        pass