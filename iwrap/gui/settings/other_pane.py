import tkinter, tkinter.ttk, logging
from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class OtherPane(tkinter.ttk.Frame, IWrapPane):

    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """
        Argument master: Parent widget from Tkinter class. Default to None.
        Attributes:
            get_state (tk.StringVar()): A name of a subroutine returning information about the internal model state.
            set_state (tk.StringVar()): A name of a subroutine restoring the internal model state.
        """
        super().__init__(master)
        self.get_state = tkinter.StringVar()
        self.set_state = tkinter.StringVar()
        self.get_timestamp = tkinter.StringVar()
        # LABEL FRAME
        labelframe = tkinter.ttk.Frame(self, height=100)
        labelframe.pack(fill=tkinter.BOTH, side=tkinter.BOTTOM, expand=1, anchor=tkinter.NW, pady=10)
        labelframe.grid_columnconfigure(1, weight=1)
        # get_state
        tkinter.ttk.Label(labelframe, text="Get state:").grid(column=0, row=1, padx=10, pady=5, sticky=(tkinter.W, tkinter.N))
        text = tkinter.ttk.Entry(labelframe, textvariable=self.get_state)
        text.grid(column=1, row=1, padx=10, pady=5, sticky=(tkinter.W, tkinter.E))
        # set_state
        tkinter.ttk.Label(labelframe, text="Set state:").grid(column=0, row=2, padx=10, pady=5, sticky=(tkinter.W, tkinter.N))
        text = tkinter.ttk.Entry(labelframe, textvariable=self.set_state)
        text.grid(column=1, row=2, padx=10, pady=5, sticky=(tkinter.W, tkinter.E))
        # get_timestamp
        tkinter.ttk.Label(labelframe, text="Get timestamp:").grid(column=0, row=3, padx=10, pady=5, sticky=(tkinter.W, tkinter.N))
        text = tkinter.ttk.Entry(labelframe, textvariable=self.get_timestamp)
        text.grid(column=1, row=3, padx=10, pady=5, sticky=(tkinter.W, tkinter.E))

    def update_settings(self, *args):
        """
        Update settings in the ProjectSettings.
        """
        code_description = ProjectSettings.get_settings().code_description
        code_description.implementation.subroutines.get_state = self.get_state.get()
        code_description.implementation.subroutines.set_state = self.set_state.get()
        code_description.implementation.subroutines.get_timestamp = self.get_timestamp.get()

    def reload(self):
        """
        Reload get_state, set_state, get_timestamp values then the project settings are changed.
        """
        code_description = ProjectSettings.get_settings().code_description
        self.get_state.set(code_description.implementation.subroutines.get_state or '')
        self.set_state.set(code_description.implementation.subroutines.set_state or '')
        self.get_timestamp.set(code_description.implementation.subroutines.get_timestamp or '')
