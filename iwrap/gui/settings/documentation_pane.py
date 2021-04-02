import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class DocumentationPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)
        # Label Frame for Actor documentation
        self.documentation_frame = tk.LabelFrame(self, text="Actor documentation")

        # Text Editor for Actor documentation
        self.documentation_editor = TextEditor(self.documentation_frame)

        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        self.documentation_editor.text = code_description

        # Pack the documentation frame
        self.documentation_frame.pack(fill=tk.BOTH, expand=True)

        # Disable editor to prevent unwanted changes
        self.documentation_editor.text_editor.config(state="disable")

    def reload(self):
        pass


class TextEditor(IWrapPane):
    def __init__(self, master=None):
        super().__init__()

        # Text content of a text editor
        self._text: str = None

        # Scrollbar for the text box widget
        self.scrollbar = ttk.Scrollbar(master)
        # Pack scrollbar
        self.scrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)

        # Status label
        self.status = tk.Label(master, text="Ready", height=1, borderwidth=1, relief="ridge", anchor="e")
        # Pack the status label
        self.status.pack(side=tk.BOTTOM, expand=False, fill=tk.X, padx=5, pady=(1, 2))

        # Text Box for the text editor
        self.text_editor = tk.Text(master)
        # Pack text box
        self.text_editor.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)

        # Configure scrollbar for text box scrolling
        self.scrollbar.config(command=self.text_editor.yview)

        # Configure callback from text box for scrollbar widget
        self.text_editor['yscrollcommand'] = self.scrollbar.set

        # Configure the test editor event bindings
        self.text_editor.bind("<FocusIn>", self.focus)
        self.text_editor.bind("<FocusOut>", self.focus)

        # Configure the text editor default style
        self.appearance(in_focus=False)

    @property
    def text(self):
        # Pull content from the text editor out of first line from zero-position character
        # to the end and delete newline character at final position.
        self._text = self.text_editor.get("1.0", tk.END+"-1c")
        return self._text

    @text.setter
    def text(self, new_text):
        # Set new content for the text editor
        self._text = new_text
        # Insert content into the text editor at first line form zero-position character
        self.text_editor.insert("1.0", self._text)

    def focus(self, event):
        if str(event) == "<FocusIn event>":
            print(self.text)
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
