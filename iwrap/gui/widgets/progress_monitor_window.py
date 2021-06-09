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
        self.minsize(400, 300)
        self.geometry('400x100')
        self.resizable(width=False, height=False)
        self.title("iWrap - actor generation progress monitor")
        tk.Label(self, text="Actor generation in progress").pack(pady=10)

        # Textbox with scrollbar
        scrollbar = ttk.Scrollbar(self)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)
        self.__text_editor = tk.Text(self)
        self.__text_editor.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)
        scrollbar.config(command=self.__text_editor.yview)
        self.__text_editor['yscrollcommand'] = scrollbar.set
        self.__text_editor.config(bg='#FFF', fg='#000', insertbackground='#000')

    def write(self, *args, **kwargs):
        self.__append_text_editor(*args)

    def __append_text_editor(self, txt):
        self.__text_editor.insert(tk.END, "\n" + txt)




