import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class DocumentationPane(ttk.Frame, IWrapPane):
    """The documentation pane tab class.

    Attributes:
        documentation_editor (TextEditor): Allows to access the text editor object outside the class.

    Properties:
        documentation (str): Documentation text.

    Notes:
        Creates a new documentation frame with a scrollable documentation text editor.
        Additionally, it creates an instance of the ProjectSettings and loads the code description 
        that can later be exported to a YAML file or imports documentation to the editor.
    """
    def __init__(self, master=None):
        """Initialize the documentation pane tab.
        Args:
            master (ttk.Frame, optional): A parent widget.
        
        Note:
            Creates a template for a text editor from the TextEditor subclass
        """
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
        Args:
            new_value (str): New text to update documentation.
        
        Note:
            Calls the documentation property setter to set new value.
        """
        self.documentation = new_value

    # Documentation getter
    @property
    def documentation(self):
        """:obj: `str`: Get or set the current documentation text stored in ProjectSettings class.
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
        """Immediately refresh the documentation editor text content.
        """
        self.documentation_editor.text = self.documentation


class TextEditor(IWrapPane):
    """Simple scrollable text editor.

    Attributes:
        command (optional): Reference to external method or function to be called after event occurs.
        text_editor (tk.Text): A text widget which handles multiple line text and is complete text editor in a window.

    Properties:
        text (str): The text stored in text editor.

    Notes:
        Three types of events trigger the callback method: `FocusIn`, `FocusOut` and `KeyRelease`.
    """
    def __init__(self, master=None, command=None):
        """Initialize the scrollable text editor.
        Args:
            master (ttk.Frame, optional): A parent widget.
            command (optional): Reference to a callback method.
        Note:
            Preconfigures the text editor widget.
        """
        super().__init__()

        # External command callback
        self.command = command

        # Text content of a text editor
        self._text: str = ""

        # Scrollbar for the text box widget
        scrollbar = ttk.Scrollbar(master)
        # Pack scrollbar
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)

        # Text Box for the text editor
        self.text_editor = tk.Text(master)
        # Pack text box
        self.text_editor.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)

        # Configure scrollbar for text box scrolling
        scrollbar.config(command=self.text_editor.yview)

        # Configure callback from text box for scrollbar widget
        self.text_editor['yscrollcommand'] = scrollbar.set

        # Configure the text editor event callbacks
        self.text_editor.bind("<FocusIn>", self.__focus_event)
        self.text_editor.bind("<FocusOut>", self.__focus_event)
        self.text_editor.bind('<KeyRelease>', self.__focus_event)

    @property
    def text(self):
        """:obj: `str`: stores the text content of the text editor. Sets and gets value from the text editor widget.
        
        Prevents from inserting non (str) type object to the widget.
        """
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

    def __focus_event(self, event):
        """A private callback method triggered by the events binding.

        Notes:
            For losing focus clears any text selection.
        
        Returns:
            Calls external command, every time it is triggered.
        """
        # Focus out of the text widget
        if str(event) == "<FocusOut event>":
            # Clear selected text if left        
            self.text_editor.selection_clear()

        # Return the text editor content to external command
        return self.command(self.text)

    def clear_text_input(self):
        """Class method for clearing all content stored in the text editor widget.
        """
        self.text_editor.delete("1.0", "end")

    def reload(self):
        """Does nothing.
        """
        pass
