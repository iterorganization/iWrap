import logging
import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.gui.settings.subroutine_pane import SubroutinePane
from iwrap.gui.settings.other_pane import OtherPane

from iwrap.gui.settings.tooltip import ToolTip


class SubroutinesPane(ttk.Frame, IWrapPane):
    """Subroutines pane contains main, init and finalize values.

    Attributes:
        init (tk.StringVar()): A name of subroutine that could be used to initialise the code
        main (tk.StringVar()): A name of the main subroutine that will be called from actor
        finalize (tk.StringVar()): A name of subroutine that could be used to finalise the code
        get_state (tk.StringVar()): A name of a subroutine returning information about the internal state of the code
        set_state (tk.StringVar()): A name of a subroutine restoring the internal state of the code
        get_timestamp (tk.StringVar()): A name of a subroutine providing time of the computed step of simulation
    """

    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the subroutines pane.

        Args:
            master: Parent widget from Tkinter class. Default to None.
        """
        super().__init__(master)

        # TABS FRAME
        tab_frame = ttk.Frame(self)
        tab_frame.pack(fill=tk.BOTH, side=tk.BOTTOM, expand=1, anchor=tk.NW)

        # NOTEBOOK WITH TABS
        tab_control = ttk.Notebook(tab_frame)
        init_tab = ttk.Frame(tab_control)
        main_tab = ttk.Frame(tab_control)
        finalize_tab = ttk.Frame(tab_control)
        other_tab = ttk.Frame(tab_control)
        tab_control.add(init_tab, text="Init:")
        tab_control.add(main_tab, text="Main:")
        tab_control.add(finalize_tab, text="Finalize:")
        tab_control.add(other_tab, text="Other:")
        tab_control.pack(fill=tk.BOTH, expand=1, anchor=tk.SW, pady=5)
        self.init_pane = SubroutinePane('init', init_tab)
        self.init_pane.pack(fill=tk.BOTH, expand=1, anchor=tk.SW)
        self.main_pane = SubroutinePane('main', main_tab)
        self.main_pane.pack(fill=tk.BOTH, expand=1, anchor=tk.SW)
        self.finalize_pane = SubroutinePane('finalize', finalize_tab)
        self.finalize_pane.pack(fill=tk.BOTH, expand=1, anchor=tk.SW)
        self.other_pane = OtherPane(other_tab)
        self.other_pane.pack(fill=tk.BOTH, expand=1, anchor=tk.SW)

    def update_settings(self, *args):
        """Update settings in the ProjectSettings.
        """
        self.init_pane.update_settings()
        self.main_pane.update_settings()
        self.finalize_pane.update_settings()
        self.other_pane.update_settings()

    def reload(self):
        """Reload init, main, and finalize values then the project settings are changed".
        """
        self.init_pane.reload()
        self.main_pane.reload()
        self.finalize_pane.reload()
        self.other_pane.reload()
