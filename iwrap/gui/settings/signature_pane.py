import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.generation.generators_mgmt import GeneratorManager


class SignaturePane(ttk.Frame, IWrapPane):
    """Pane composed of a multiline, read-only and two axis scrollable text field with ButtonsBarPane above to control.

        Attributes:
            text_box (tk.Text): Text widget that displays multiline text in read-only mode.
    """
    def __init__(self, master=None):
        """Initialize the SignaturePane frame.
        Args:
            master (ttk.Frame, optional): A parent widget.
        """
        super().__init__(master)

        # Buttons bar to control pane's content
        ButtonsBarPane(self)
        # Text box widget
        self.text_box: TextBox = TextBox(self)

    def update_settings(self):
        pass

    def reload(self):
        """Calls the refresh method of the TextBox."""
        self.text_box.refresh()


class TextBox(ttk.Frame):
    """Widget consisting of a text field with read-only mode and two scroll bars (X and Y).

        Attributes:
            text_box (tk.Text): Text widget that displays multiline text in read-only mode.
    """
    def __init__(self, master=None) -> None:
        """Initialize the text box widget in readonly mode.
        Args:
            master (ttk.Frame, optional): A parent widget.
        Note:
            Preconfigures the appearance of the text box widget.
        """

        super().__init__(master)

        # Scrollbars for the text box widget
        vertical_scroll = ttk.Scrollbar(master)
        horizontal_scroll = ttk.Scrollbar(master)
        # Pack vertical_scroll and horizontal_scroll
        vertical_scroll.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)
        horizontal_scroll.pack(side=tk.BOTTOM, fill=tk.X, pady=(5, 2), padx=5)

        # Text Box for the text box
        self.text_box = tk.Text(master)
        # Configure to none wrapping text & disable editing.
        self.text_box.config(wrap=tk.NONE, state=tk.DISABLED)
        # Pack text box
        self.text_box.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)

        # Configure Scrollbars for text box scrolling
        vertical_scroll.config(orient=tk.VERTICAL,
                               command=self.text_box.yview)
        horizontal_scroll.config(orient=tk.HORIZONTAL,
                                 command=self.text_box.xview)

        # Configure callback from text box for Scrollbars widgets
        self.text_box['yscrollcommand'] = vertical_scroll.set
        self.text_box['xscrollcommand'] = horizontal_scroll.set

        # Pre-configure the text box appearance
        self.text_box.config(bg='#EFEFEF', fg='#000', insertbackground='#000')

    def refresh(self) -> None:
        """Clears the contents of the text box and inserts new text data."""

        # The text_box widget has to be in tk.NORMAL state to allow text insertion.
        self.text_box.config(state=tk.NORMAL)
        # Clear current content.
        self.clear()
        # Insert new text data to the text box.
        self.text_box.insert("1.0", self.data_load())
        # Back to tk.DISABLED state.
        self.text_box.config(state=tk.DISABLED)

    @staticmethod
    def data_load() -> str:
        """Loads text data from an external generator."""
        GeneratorManager.init_generator(None, None)
        return GeneratorManager.get_code_signature()

    def clear(self) -> None:
        """Removes text from a text box."""
        self.text_box.delete('1.0', tk.END)

    def get_text(self) -> str:
        """Returns text from a text box."""
        return self.text_box.get("1.0", tk.END)


class ButtonsBarPane(ttk.Frame):
    """Widget which is a bar of control buttons."""
    def __init__(self, master: ttk.Widget = None) -> None:
        """Initialize the ttk.Button widgets.
        Args:
            master (ttk.Frame, optional): A parent widget.
        """
        super().__init__(master)

        # First button to execute copy to clipboard action.
        ttk.Button(self, text="Copy to clipboard", command=self.copy_to_clipboard).pack(side=tk.LEFT)
        # Temporary button to execute reload method of the parent widget.
        ttk.Button(self, text="Refresh", command=self.master.reload).pack(side=tk.LEFT, padx=5)

        # ButtonBarPane object pack configuration.
        self.pack(expand=False, fill=tk.X, padx=5, pady=(5, 0))

    def copy_to_clipboard(self):
        """Executes 'Copy to clipboard' action."""
        content = self.master.text_box.get_text()
        self.clipboard_clear()
        self.clipboard_append(content)
