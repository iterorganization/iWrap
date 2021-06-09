import tkinter as tk
from tkinter import ttk
from io import TextIOBase


class ProgressMonitorWindow(tk.Toplevel, TextIOBase):
    def __init__(self):
        tk.Toplevel.__init__(self)
        self.text_editor = None
        self.initialize_window()

    def initialize_window(self):
        self.transient(self.master)
        self.minsize(400, 300)
        self.geometry('400x100')
        self.resizable(width=False, height=False)
        self.title("iWrap - actor generation progress monitor")
        tk.Label(self, text="Actor generation in progress").pack(pady=10)

        # Textbox with scrollbar
        scrollbar = ttk.Scrollbar(self)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)
        self.text_editor = tk.Text(self)
        self.text_editor.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)
        scrollbar.config(command=self.text_editor.yview)
        self.text_editor['yscrollcommand'] = scrollbar.set
        self.text_editor.config(bg='#FFF', fg='#000', insertbackground='#000')

    def write(self, *args, **kwargs):
        ...




