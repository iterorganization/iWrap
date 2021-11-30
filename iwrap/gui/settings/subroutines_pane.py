import logging
import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class SubroutinesPane(ttk.Frame, IWrapPane):
    """Subroutines pane contains main, init and finalize values.

    Attributes:
        init (tk.StringVar()): A name of subroutine that could be used to initialise the native code
        main (tk.StringVar()): A name of the main subroutine that will be called from actor
        finalize (tk.StringVar()): A name of subroutine that could be used to finalise the native code
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the subroutines pane.

        Args:
            master: Parent widget from Tkinter class. Default to None.
        """
        super().__init__(master)
        self.main = tk.StringVar()
        self.init = tk.StringVar()
        self.finalize = tk.StringVar()

        # SUBROUTINES LABEL FRAME
        labelframe_sub = ttk.Frame(self, height=100)
        labelframe_sub.pack(fill=tk.BOTH, side=tk.BOTTOM, expand=1, anchor=tk.NW, pady=10)
        labelframe_sub.grid_columnconfigure(1, weight=1)

        # INIT
        ttk.Label(labelframe_sub, text="Init:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        text = ttk.Entry(labelframe_sub, textvariable=self.init)
        text.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))

        # MAIN
        ttk.Label(labelframe_sub, text="*Main:").grid(column=0, row=2, padx=10, pady=5, sticky=(tk.W, tk.N))
        text = ttk.Entry(labelframe_sub, textvariable=self.main)
        text.grid(column=1, row=2, padx=10, pady=5, sticky=(tk.W, tk.E))

        # Finalize
        ttk.Label(labelframe_sub, text="Finalize:").grid(column=0, row=3, padx=10, pady=5, sticky=(tk.W, tk.N))
        text = ttk.Entry(labelframe_sub, textvariable=self.finalize)
        text.grid(column=1, row=3, padx=10, pady=5, sticky=(tk.W, tk.E))

    def update_settings(self, *args):
        """Update settings in the ProjectSettings.
        """
        code_description = ProjectSettings.get_settings().code_description
        code_description.implementation.subroutines.main = self.main.get()
        code_description.implementation.subroutines.finalize = self.finalize.get()
        code_description.implementation.subroutines.init = self.init.get()

    def reload(self):
        """Reload init, main, and finalize values then the project settings are changed".
        """
        code_description = ProjectSettings.get_settings().code_description
        self.main.set(code_description.implementation.subroutines.main or '')
        self.finalize.set(code_description.implementation.subroutines.finalize or '')
        self.init.set(code_description.implementation.subroutines.init or '')
