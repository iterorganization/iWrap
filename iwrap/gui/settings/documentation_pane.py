import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class DocumentationPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)
        self.documentation_editor = TextEditor(self)

    def reload(self):
        pass


class TextEditor(IWrapPane):
    def __init__(self, master=None):
        super().__init__()

        # Scrollbar for the text box widget
        self.scrollbar = tk.Scrollbar(master)
        # Pack scrollbar
        self.scrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)

        # Text Box for the text editor
        self.text_editor = tk.Text(master)
        # Pack text box
        self.text_editor.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)

        # Status label
        self.status = tk.Label(master, text="Ready", height=1, borderwidth=1, relief="ridge", anchor="e")
        self.status.pack(side=tk.BOTTOM, expand=False, fill=tk.X, padx=5, pady=(1, 2))

        # Configure scrollbar for text box scrolling
        self.scrollbar.config(command=self.text_editor.yview)

        # Configure callback from text box for scrollbar widget
        self.text_editor['yscrollcommand'] = self.scrollbar.set

        # Configure the test editor event bindings
        self.text_editor.bind("<FocusIn>", self.focus)
        self.text_editor.bind("<FocusOut>", self.focus)

        # Configure the text editor default style
        self.appearance(in_focus=False)

    def focus(self, event):
        if str(event) == "<FocusIn event>":
            self.appearance(in_focus=True)
            return self.status.config(text="Edit mode")
        self.appearance(in_focus=False)
        return self.status.config(text="Ready")

    def appearance(self, in_focus):
        if in_focus:
            return self.text_editor.config(bg='#ffffff', fg='#000', selectbackground='#0099FF', selectforeground='#FFFFFF')
        self.text_editor.selection_clear()
        return self.text_editor.config(bg='#EDEDED', fg='#404040')

    def reload(self):
        pass
