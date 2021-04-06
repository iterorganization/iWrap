import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class DocumentationPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        """Initilize new documentation pane. It takes as nonobligatory argument
         the parent widget from tkinter class. In return it creates a new documentation frame 
         with a scrollable documentation text editor.
         Additionally, it creates an instance of the project settings and loads the code description 
         that can later be exported to a YAML file. 
         Alternatively, it places the imported documentation in the editor."""
        super().__init__(master)

        # Documentation property instance
        self.documentation: str = ""

        # Label Frame for Actor documentation
        documentation_frame = tk.LabelFrame(self, text="Actor documentation")

        # Text Editor for Actor documentation
        self.documentation_editor = TextEditor(documentation_frame, self.update_documentation)

        # Pack the documentation frame
        documentation_frame.pack(fill=tk.BOTH, expand=True)

        # Execute initial reload
        self.reload()

    def update_documentation(self, new_value: str) -> None:
        """Callback method to update the documentation property.
        Takes 1 positional argument
            new_value: str
        Then calls property to set new value.
        """
        self.documentation = new_value

    # Documentation getter
    @property
    def documentation(self):
        """A property that allows you to store a sequence of documentation. Gets and sets
         the value of the documentation stored in the ProjectSettings class.
        """
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        return code_description.documentation

    @documentation.setter
    def documentation(self, new_documentation):
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        code_description.documentation = new_documentation
    
    def reload(self):
        """Reload method, when called it immediately refresh the documentation editor text content.
        """
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
        # Focus out of the text widget
        if str(event) == "<FocusOut event>":
            # Clear selected text if left        
            self.text_editor.selection_clear()

        # Return the text editor content to external command
        return self.command(self.text)

    def clear_text_input(self):
        self.text_editor.delete("1.0", "end")

    def reload(self):
        pass
