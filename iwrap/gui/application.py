import tkinter as tk
from tkinter import ttk
import importlib
from importlib import resources
from tkinter.constants import NO, S
from typing import cast

from iwrap.gui.actor_description import ActorDescriptionPane
from iwrap.gui.generics import IWrapPane
from iwrap.gui.menu import MenuBar

from iwrap.gui.settings.main_pane import SettingsMainPane


class ButtonPane( ttk.Frame ):
    def __init__(self, master: ttk.Widget):
        super().__init__(master, borderwidth=1, relief="solid")

        close_button = ttk.Button( self, text='Close', command=self.winfo_toplevel().destroy )
        close_button.pack( side=tk.RIGHT, padx=10, pady=5 )

        generate_button = ttk.Button( self, text='Generate', command=None )
        generate_button.pack( side=tk.RIGHT, padx=10, pady=5 )


class MainWindow(tk.Tk, IWrapPane):
    def __init__(self):
        super().__init__()

        self.title("iWrap")
        self.minsize(600, 300)
        self.geometry('600x600')
        # Sets application icon
        self._icon = self.__set_icon()

        self.config(menu=MenuBar(self))

        main_pane = ttk.Frame(self)
        main_pane.pack(fill=tk.BOTH, expand=True)

        # Frame to hold ActorDescriptionPane nad LogoPane
        top_pane = ttk.Frame(main_pane)
        top_pane.pack(fill=tk.X, expand=False, side=tk.TOP)

        self.actor_description = ActorDescriptionPane(top_pane)
        self.actor_description.pack(fill=tk.BOTH, side=tk.LEFT, padx=5, pady=5, expand=True)

        # Load IMAS logo next to the ActorDescriptionPane
        self.img: tk.PhotoImage = tk.PhotoImage()
        self.__load_logo()
        # Put logo as label into ActorDescriptionPane grid
        ttk.Label(top_pane, image=self.img).pack(side=tk.RIGHT, padx=(0, 5), pady=(10, 0))

        button_pane = ButtonPane(main_pane)
        button_pane.pack(fill=tk.X, side=tk.BOTTOM)

        self.settings_pane = SettingsMainPane(main_pane)
        self.settings_pane.pack(fill=tk.BOTH, side=tk.TOP, expand=True)

        self.center()

    def __load_logo(self):
        package = "iwrap.resources"
        resource = "imas_transparent_logo.gif"
        # Try to access icon path using importlib.resource module
        try:
            with importlib.resources.path(package, resource) as icon_path:
                img = tk.PhotoImage(file=icon_path)
                img = img.subsample(6, 6)
                self.img = img
        # Rise Import Error
        except ImportError:
            pass

    def update_settings(self):
        self.actor_description.update_settings()
        self.settings_pane.update_settings()

    def reload(self):
        self.actor_description.reload()
        self.settings_pane.reload()

    def center(self):
        self.update_idletasks()
        screen_width = self.winfo_screenwidth()
        screen_height = self.winfo_screenheight()

        size = tuple( int( _ ) for _ in self.geometry().split( '+' )[0].split( 'x' ) )
        x = screen_width / 2 - size[0] / 2
        y = screen_height / 2 - size[1] / 2

        self.geometry( "+%d+%d" % (x, y) )
    
    def __set_icon(self) -> tk.PhotoImage:
        package = "iwrap.resources"
        resource = "imas_logo_round.gif"
        # Try to access icon path using importlib.resource module
        try:
            with importlib.resources.path(package, resource) as icon_path:
                icon = tk.PhotoImage(file=icon_path)
                self.iconphoto(True, icon)
                return icon
        # Rise Import Error
        except ImportError:
            return None


if __name__ == '__main__':
    window = MainWindow()

    window.mainloop()
