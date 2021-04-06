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
        self.documentation_editor = TextEditor(self.documentation_frame, self.update_documentation)

        # Pack the documentation frame
        self.documentation_frame.pack(fill=tk.BOTH, expand=True)

        # Execute initial reload
        self.reload()

    def update_documentation(self, new_value) -> None:
        self.documentation = new_value

    # Documentation getter
    @property
    def documentation(self):
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        return code_description.documentation

    @documentation.setter
    def documentation(self, new_documentation):
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        code_description.documentation = new_documentation

    def reload(self):
        self.documentation_editor.text = self.documentation


class TextEditor(IWrapPane):
    def __init__(self, master=None, command=None):
        super().__init__()

        # External command callback
        self.command = command

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

        # Configure the text editor event callbacks
        self.text_editor.bind("<FocusIn>", self.focus)
        self.text_editor.bind("<FocusOut>", self.focus)
        self.text_editor.bind('<KeyRelease>', self.focus)

    @property
    def text(self):
        # Pull content from the text editor out of first line from zero-position character
        # to the end and delete newline character at final position.
        self._text = self.text_editor.get("1.0", tk.END+"-1c")
        return self._text

    @text.setter
    def text(self, new_text):
        # In case of file import error
        if not isinstance(new_text, str):
            # Fill with blank text
            new_text=""

        # Set new content to the text container
        self._text = new_text

        # Insert content into the text editor at first line from zero-position character
        # But clear the text widget from leftovers first
        self.clear_text_input()
        self.text_editor.insert("1.0", self._text)

    def focus(self, event):
        # Focus in the text widget
        if str(event) == "<FocusIn event>":
            return self.status.config(text="Edit mode")

        # Clear selected text if left        
        self.text_editor.selection_clear()

        # Pass the text editor content to external command
        self.command(self.text)

        return self.status.config(text="Ready")

    def clear_text_input(self):
        self.text_editor.delete("1.0", "end")

    def reload(self):
        pass
