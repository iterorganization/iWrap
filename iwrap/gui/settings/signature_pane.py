import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane
from iwrap.generation.generators_mgmt import GeneratorManager


class SignaturePane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)

        ButtonBarPane(self)
        self.text_box: TextBox = TextBox(self)

    def update_settings(self):
        pass

    def reload(self):
        self.text_box.refresh()


class TextBox(ttk.Frame):
    def __init__(self, master=None) -> None:
        super().__init__(master)

        # Scrollbar for the text box widget
        scrollbar = ttk.Scrollbar(master)
        # Pack scrollbar
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, pady=(5, 2), padx=2)

        # Text Box for the text box
        self.text_box = tk.Text(master)
        # Pack text box
        self.text_box.pack(side=tk.TOP, expand=True, fill=tk.BOTH, pady=(5, 2), padx=5)

        # Configure scrollbar for text box scrolling
        scrollbar.config(command=self.text_box.yview)

        # Configure callback from text box for scrollbar widget
        self.text_box['yscrollcommand'] = scrollbar.set

        # Pre-configure the text box appearance
        self.text_box.config(bg='#FFF', fg='#000', insertbackground='#000')

    def refresh(self):
        self.clear()
        GeneratorManager.init_generator(None, None)
        text = GeneratorManager.get_code_signature()
        self.text_box.insert("1.0", text)

    def clear(self):
        self.text_box.delete('1.0', tk.END)

    def get_text(self) -> str:
        return self.text_box.get("1.0", tk.END)


class ButtonBarPane(ttk.Frame):
    def __init__(self, master: ttk.Widget = None) -> None:
        super().__init__(master)

        ttk.Button(self, text="Copy to clipboard", command=self.copy_to_clipboard).pack(side=tk.LEFT)
        ttk.Button(self, text="Refresh", command=self.master.reload).pack(side=tk.LEFT)

        self.pack(expand=False, fill=tk.X, padx=5)

    def copy_to_clipboard(self):
        content = self.master.text_box.get_text()
        self.clipboard_clear()
        self.clipboard_append(content)
