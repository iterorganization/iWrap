import tkinter as tk
from tkinter import LabelFrame, ttk
from tkinter.constants import S

from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class DocumentationPane(ttk.LabelFrame, IWrapPane):
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
        super().__init__(master, text="Actor documentation")

        # Text Editor for Actor documentation
        self.documentation_editor = TextEditor(self)
    
    def update_settings(self):
        """Update documentation in ProjectSettings.
        Args:
            update_value (str, optional): Text to update in ProjectSettings.
        """
        ProjectSettings.get_settings().code_description.documentation = None

    def reload(self):
        """Immediately refresh the documentation editor text content.
        """
        self.documentation_editor.reload()


class TextEditor:
    """Simple scrollable text editor.

    Attributes:
        _master (optional): (Private) Reference to parent object.
        text_editor (tk.Text): A text widget which handles multiple line text and is complete text editor in a window.

    Properties:
        text (str): The text stored in text editor.

    Notes:
        One type of event triggers the callback method: `FocusOut`.
    """
    def __init__(self, master: ttk.Widget=None):
        """Initialize the scrollable text editor.
        Args:
            master (ttk.Frame, optional): A parent widget.
        Note:
            Preconfigures the text editor widget.
        """
        super().__init__()

        # Parent attribute
        self._master = master

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
        self.text_editor.bind('<FocusOut>', self.focus_lost_event)

        # Pre-configure the text editor appearance
        self.text_editor.config(bg='#FFF', fg='#000', insertbackground='#000')

        # Initial reload of an text editor
        self.reload()

    @property
    def text(self):
        """:obj: `str`: stores the text content of the text editor. Sets and gets value from the text editor widget.
        
        Prevents from inserting non (str) type object to the widget.
        """
        # Pull content from the text editor out of first line from zero-position character
        # to the end and delete newline character at final position.
        self._text = self.text_editor.get('1.0', tk.END+'-1c')
        return self._text

    @text.setter
    def text(self, new_text: str):
        # In case of file import error
        if not isinstance(new_text, str):
            # Fill with blank text
            new_text=""

        # Set new content to the text container
        self._text = new_text

        # Insert content into the text editor at first line from zero-position character
        # But clear the text widget from leftovers first
        self.clear_text_input()
        self.text_editor.insert('1.0', self._text)

    def focus_lost_event(self, event):
        """A private callback method triggered by the event binding.

        Gets text property content and sets parent documentation property.

        Notes:
            For losing focus clears any text selection.
        """
        # Clear selected text    
        self.text_editor.selection_clear()

    def clear_text_input(self):
        """Class method for clearing all content stored in the text editor widget.
        """
        self.text_editor.delete('1.0', tk.END)

    def reload(self) -> None:
        project_settings = ProjectSettings.get_settings()
        code_description = project_settings.code_description
        self.text = code_description.documentation
