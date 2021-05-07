import tkinter as tk
from tkinter import ttk

from typing import Union

from iwrap.generation_engine.engine import Engine
from iwrap.gui.generics import IWrapPane


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

        # SignaturePane index in the parent notebook
        self._index: Union[int, None] = None

        # Text box declaration
        self.text_box: TextBox

        # Buttons bar to control pane's content
        ButtonsBarPane(self)
        # Text box widget initialization
        self.text_box = TextBox(self)

    @property
    def tab_index(self):
        """Get/Set the SignaturePane index of the parent notebook."""
        return self._index

    @tab_index.setter
    def tab_index(self, value):
        self._index = value

    def update_settings(self):
        pass

    def reload(self):
        """Calls the refresh method of the TextBox."""
        self.text_box.refresh()


class TextBox(ttk.LabelFrame):
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

        super().__init__(master, text="Wrapped code expected signature:")

        # Scrollbars for the text box widget
        vertical_scroll = ttk.Scrollbar(self)
        horizontal_scroll = ttk.Scrollbar(self)
        # Pack vertical_scroll and horizontal_scroll
        vertical_scroll.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)
        horizontal_scroll.pack(side=tk.BOTTOM, fill=tk.X, pady=(5, 2), padx=5)

        # Text Box for the text box
        self.text_box = tk.Text(self)
        # Configure to none wrapping text & disable editing.
        self.text_box.config(wrap=tk.NONE, state=tk.DISABLED)
        # Configure selection color
        self.text_box.config(selectbackground="#D3E2FC", inactiveselectbackground="#E6EFFD")
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

        # Pop-up menu:
        popup_menu = self.PopUpPane(self.text_box)
        # Bind button press to rise popup menu.
        self.text_box.bind("<Button-2>", popup_menu.popup_rise)
        self.text_box.bind("<Button-3>", popup_menu.popup_rise)

        # Pack TextBox.
        self.pack(expand=True, fill=tk.BOTH)

    class PopUpPane(tk.Menu):
        def __init__(self, master: tk.Widget = None) -> None:
            """Initialize the popup menu widget.
            Args:
                master (tk.Widget, optional): A parent widget.
            """
            super().__init__(master)

            # Configure the menu commands.
            self.add_command(label="Copy", command=self.copy)
            self.add_separator()
            self.add_command(label="Refresh...", command=self.refresh)
            self.add_command(label="Select All", command=self.select_all)

            # Configure: turn off the tear off option.
            self.config(tearoff=False)

        def popup_rise(self, event=None) -> None:
            """Handles popup rising."""
            # Make sure the widget is in focus
            event.widget.focus_set()
            try:
                # Pop the menu at given coordinates.
                self.tk_popup(event.x_root, event.y_root)
            finally:
                self.grab_release()

        def copy(self) -> None:
            """Copies the selection."""
            self.master.event_generate("<<Copy>>")
            print()

        def refresh(self) -> None:
            """Calls external refresh method."""
            self.master.master.refresh()

        def select_all(self) -> None:
            """Selects all text."""
            self.master.tag_add(tk.SEL, "1.0", tk.END)

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
        return Engine().get_code_signature()

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

        # ButtonBarPane object pack configuration.
        self.pack(expand=False, fill=tk.X, padx=5, pady=(10, 5))

    def copy_to_clipboard(self):
        """Executes 'Copy to clipboard' action."""
        content = self.master.text_box.get_text()
        self.clipboard_clear()
        self.clipboard_append(content)
