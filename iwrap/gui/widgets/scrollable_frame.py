import logging
import tkinter as tk
from tkinter import ttk


class ScrollableFrame( ttk.Frame ):
    """Scrollable frame widget.

    Attributes:
        scrollbar(Scrollbar): Slide controller that is used to implement vertically scrolled Canvas.
        canvas(Canvas): Area intended for placing scrollbar and frame.

    ``Example``:

    .. code-block:: python

        your_scrollable_frame = ScrollableFrame(root)
        Label(your_scrollable_frame, text="Hello world!").pack()
        your_scrollable_frame = your_scrollable_frame.update()

    """
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master):
        """ Initialize the scrollable frame.

        Args:
            master: Parent widget from Tkinter class. Default to None.
        """
        scrollbar = tk.Scrollbar(master)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, expand=0)

        self.canvas = tk.Canvas(master, yscrollcommand=scrollbar.set)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=1)
        scrollbar.config(command=self.canvas.yview)
        self.canvas.bind('<Configure>', self._fill_canvas)

        ttk.Frame.__init__(self, master)
        self.windows_item = self.canvas.create_window(0, 0, window=self, anchor=tk.NW)

    def _fill_canvas(self, event):
        """The method is called always when configuring resources of a widget are changed.

        Args:
            event(Event): Configure resources of canvas event.
        """
        self.canvas.itemconfig(self.windows_item, width=event.width)

    def update(self):
        """Update canvas scrollable region when something new is added to the scrollable frame.

        Note:
            The update() should be called when something new is added to the scrollable frame.
        """
        self.update_idletasks()
        self.canvas.config(scrollregion=self.canvas.bbox(self.windows_item))
