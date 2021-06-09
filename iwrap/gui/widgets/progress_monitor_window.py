import tkinter as tk
from tkinter import ttk
from io import TextIOBase


class ProgressMonitorWindow(tk.Toplevel, TextIOBase):
    def __init__(self):
        tk.Toplevel.__init__(self)
        self.__text_editor = None
        self.__initialize_window()

    def __initialize_window(self):
        self.transient(self.master)
        self.minsize(800, 600)
        self.geometry('800x600')
        self.resizable(width=True, height=False)
        self.title("iWrap - actor generation progress monitor")
        tk.Label(self, text="Actor generation in progress").pack(pady=10)

        # Textbox with scrollbar
        yscrollbar = ttk.Scrollbar(self)
        yscrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)
        xscrollbar = ttk.Scrollbar(self, orient=tk.HORIZONTAL)
        xscrollbar.pack(side=tk.BOTTOM, fill=tk.X, pady=(5, 2), padx=2)
        self.__text_editor = tk.Text(self, wrap=tk.NONE)
        self.__text_editor.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)
        yscrollbar.config(command=self.__text_editor.yview)
        xscrollbar.config(command=self.__text_editor.xview)
        self.__text_editor['yscrollcommand'] = yscrollbar.set
        self.__text_editor['xscrollcommand'] = xscrollbar.set
        self.__text_editor.config(bg='#FFF', fg='#000', insertbackground='#000')

        # Readonly textbox - bind key press event
        self.__text_editor.bind("<Key>", lambda e: "break")

    def write(self, *args, **kwargs):
        self.__append_text_editor(*args)

    def __append_text_editor(self, txt):
        self.__text_editor.insert(tk.END, txt)
        self.update()







