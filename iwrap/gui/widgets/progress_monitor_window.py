import logging
import tkinter as tk
from tkinter import ttk
from io import TextIOBase


class ProgressMonitorWindow(tk.Toplevel, TextIOBase):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self):
        tk.Toplevel.__init__(self)
        self.__text_editor = None
        self.__label_text = tk.StringVar(self, "Actor generation in progress")
        self.__initialize_window()
        self.__yscroll_touched = False

    def __set_yscrollbar(self, *args):
        self.__text_editor.yview(*args)
        self.__yscroll_touched = True

    def __initialize_window(self):
        self.transient(self.master)
        self.minsize(800, 600)
        self.geometry('800x600')
        self.resizable(width=True, height=False)
        self.title("iWrap - actor generation progress monitor")
        tk.Label(self, text=self.__label_text.get(),
                 textvariable=self.__label_text).pack(pady=10)

        # Textbox with scrollbar
        yscrollbar = ttk.Scrollbar(self)
        yscrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)
        xscrollbar = ttk.Scrollbar(self, orient=tk.HORIZONTAL)
        xscrollbar.pack(side=tk.BOTTOM, fill=tk.X, pady=(5, 2), padx=2)
        self.__text_editor = tk.Text(self, wrap=tk.NONE)
        self.__text_editor.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)
        yscrollbar.config(command=self.__set_yscrollbar)
        xscrollbar.config(command=self.__text_editor.xview)
        self.__text_editor['yscrollcommand'] = yscrollbar.set
        self.__text_editor['xscrollcommand'] = xscrollbar.set
        self.__text_editor.config(bg='#FFF', fg='#000', insertbackground='#000')

        # Readonly textbox - bind key press event
        self.__text_editor.bind("<Key>",
                                lambda e: self.__append_text_editor("Interruption not implemented.\n"))
        # Make window modal
        self.grab_set()

    def write(self, *args, **kwargs):
        self.__append_text_editor(*args)
            
    def __append_text_editor(self, txt):
        self.__text_editor.insert(tk.END, txt)
        if not self.__yscroll_touched:
            self.__text_editor.see( tk.END )
        self.update_idletasks()

    def set_label(self, txt):
        self.__label_text.set(txt)



