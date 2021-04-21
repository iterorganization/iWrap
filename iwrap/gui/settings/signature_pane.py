import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.generation.generators_mgmt import GeneratorManager


class SignaturePane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)

        # Buttons bar to control pane's content
        ButtonBarPane(self)
        # Text box widget
        self.text_box: TextBox = TextBox(self)

    def update_settings(self):
        pass

    def reload(self):
        self.text_box.refresh()


class TextBox(ttk.Frame):
    def __init__(self, master=None) -> None:
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
        self.text_box.config(bg='#FFF', fg='#000', insertbackground='#000')

    def refresh(self):
        GeneratorManager.init_generator(None, None)
        text = GeneratorManager.get_code_signature()
        # The text_box widget has to be in tk.NORMAL state to allow text insertion.
        self.text_box.config(state=tk.NORMAL)
        # Clear current content
        self.clear()
        self.text_box.insert("1.0", text)
        # Back to tk.DISABLED state.
        self.text_box.config(state=tk.DISABLED)

    def clear(self):
        self.text_box.delete('1.0', tk.END)

    def get_text(self) -> str:
        return self.text_box.get("1.0", tk.END)


class ButtonBarPane(ttk.Frame):
    def __init__(self, master: ttk.Widget = None) -> None:
        super().__init__(master)

        ttk.Button(self, text="Copy to clipboard", command=self.copy_to_clipboard).pack(side=tk.LEFT)
        ttk.Button(self, text="Refresh", command=self.master.reload).pack(side=tk.LEFT, padx=5)

        self.pack(expand=False, fill=tk.X, padx=5)

    def copy_to_clipboard(self):
        content = self.master.text_box.get_text()
        self.clipboard_clear()
        self.clipboard_append(content)
