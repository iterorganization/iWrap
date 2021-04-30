import tkinter as tk
from tkinter import ttk


class ScrollableFrame( ttk.Frame ):
    def __init__(self, master):
        scrollbar = tk.Scrollbar(master)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, expand=0)

        self.canvas = tk.Canvas(master, yscrollcommand=scrollbar.set)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=1)
        scrollbar.config(command=self.canvas.yview)
        self.canvas.bind('<Configure>', self._fill_canvas)

        ttk.Frame.__init__(self, master)
        self.windows_item = self.canvas.create_window(0, 0, window=self, anchor=tk.NW)

    def _fill_canvas(self, event):
        self.canvas.itemconfig(self.windows_item, width=event.width)

    def update(self):
        self.update_idletasks()
        self.canvas.config(scrollregion=self.canvas.bbox(self.windows_item))
