import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class DocumentationPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)
        #ttk.Label(self, text='Place for DOCUMENTATION', borderwidth=1, relief="solid").pack(fill=tk.BOTH, expand=1)
        self.documentation_editor = TextEditor(self)

    def reload(self):
        pass


class TextEditor(IWrapPane):
    def __init__(self, master=None):
        super().__init__()

        # Text Box for the text editor
        self.text_editor = tk.Text(master)
        # Pack text box
        self.text_editor.pack(side=tk.LEFT, fill=tk.BOTH)

        # Scrollbar for the text box widget
        self.scrollbar = tk.Scrollbar(master)
        # Pack scrollbar
        self.scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        # Configure scrollbar for text box scrolling
        self.scrollbar.config(command=self.text_editor.yview)

        # Configure callback from text box for scrollbar widget
        self.text_editor['yscrollcommand'] = self.scrollbar.set

    def reload(self):
        pass
