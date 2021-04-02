import tkinter as tk
from tkinter import ttk
import tkinter.filedialog

from iwrap.gui.generics import IWrapPane


class CodeSettingsPane(ttk.Frame, IWrapPane):
    def __init__(self, master=None):
        super().__init__(master)

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="User code settings")
        labelframe.pack(fill="both", expand="yes", pady=10)
        labelframe.grid_columnconfigure(1, weight=1)

        # COMBOBOX
        ttk.Label(labelframe, text="Programming language:").grid(column=0, row=0, padx=10, pady=5, sticky=(tk.W, tk.N))
        programming_language_combobox = ttk.Combobox(labelframe, state='readonly')
        programming_language_combobox['values'] = (' Fortran', ' C++', ' Python')
        programming_language_combobox.grid(column=1, row=0, padx=10, pady=5, sticky=(tk.W, tk.E))

        # BROWSE BUTTON AND ENTRY FOR PATH
        ttk.Label(labelframe, text="Source Code:").grid(column=0, row=1, padx=10, pady=5, sticky=(tk.W, tk.N))
        self.browse_text = tk.Entry(labelframe)
        self.browse_text.grid(column=1, row=1, padx=10, pady=5, sticky=(tk.W, tk.E))
        ttk.Button(labelframe, text="Browse", command=self.on_click, width=10)\
            .grid(row=1, column=2, padx=10, pady=5)

    def reload(self):
        pass

    def on_click(self):
        filename = tk.filedialog.askopenfilename()
        self.browse_text.insert(tk.END, filename)
