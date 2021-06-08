import tkinter as tk
from tkinter import ttk
import importlib
from importlib import resources
from typing import cast, Tuple

from iwrap.generation_engine.engine import Engine
from iwrap.gui.actor_description import ActorDescriptionPane
from iwrap.gui.generics import IWrapPane
from iwrap.gui.menu import MenuBar

from iwrap.gui.settings.main_pane import SettingsMainPane


class ButtonPane(ttk.Frame):
    def __init__(self, master: ttk.Widget):
        super().__init__(master, borderwidth=1, relief="solid")
        self.master = master
        close_button = ttk.Button(self, text='Close', command=self.winfo_toplevel().destroy)
        close_button.pack(side=tk.RIGHT, padx=10, pady=5)

        generate_button = ttk.Button(self, text='Generate', command=self.generate_action)
        generate_button.pack(side=tk.RIGHT, padx=10, pady=5)

    def generate_action(self):

        window = tk.Toplevel( self.master )
        window.transient( self.master)
        window.minsize( 400, 100 )
        window.geometry( '400x100' )
        window.resizable( width=False, height=False )
        window.title( "iWrap - actor generation" )

        label = ttk.Label( window)
        label.configure(font=('Times, 12'))
        label.configure(justify=tk.CENTER, anchor=tk.CENTER)
        label.configure(text='Generating an actor!')
        label.pack(fill=tk.BOTH, expand=True)

        label = ttk.Label( window)
        label.configure(font=('Times, 10'))
        label.configure(justify=tk.CENTER, anchor=tk.CENTER)
        label.configure(text='Please be patient!')
        label.pack(fill=tk.BOTH, expand=True)

        self.update_idletasks()

        Engine().generate_actor()
        window.destroy()


class MainWindow(tk.Tk, IWrapPane):
    def __init__(self):
        super().__init__()

        self.title("iWrap")
        self.minsize(600, 300)
        self.geometry('600x600')

        # Sets application icon
        _icon = self._load_image(resource="imas_logo_round.gif")
        self.iconphoto(True, _icon)

        self.config(menu=MenuBar(self))

        main_pane = ttk.Frame(self)
        main_pane.pack(fill=tk.BOTH, expand=True)

        # Frame to hold ActorDescriptionPane nad LogoPane
        top_pane = ttk.Frame(main_pane)
        top_pane.pack(fill=tk.X, expand=False, side=tk.TOP)

        self.actor_description = ActorDescriptionPane(top_pane)
        self.actor_description.pack(fill=tk.BOTH, side=tk.LEFT, padx=5, pady=5, expand=True)

        # Load the IMAS logo next to the ActorDescriptionPane, use the themed Label for transparency
        self._logo_img: tk.PhotoImage = self._load_image(resource="imas_transparent_logo.gif", resize=(True, (6, 6)))
        ttk.Label(top_pane, image=self._logo_img).pack(side=tk.RIGHT, padx=(15, 20), pady=(10, 0))

        button_pane = ButtonPane(main_pane)
        button_pane.pack(fill=tk.X, side=tk.BOTTOM)

        self.settings_pane = SettingsMainPane(main_pane)
        self.settings_pane.pack(fill=tk.BOTH, side=tk.TOP, expand=True)

        self.center()


    def _load_image(self,
                     package: str = "iwrap.resources",
                     resource: str = "",
                     resize: Tuple[bool, Tuple[int, int]] = (False,)) -> tk.PhotoImage:
        # Try to access image path using importlib.resource module
        try:
            with importlib.resources.path(package, resource) as img_path:
                img = tk.PhotoImage(file=img_path)
                if resize[0]:
                    img = img.subsample(resize[1][0], resize[1][1])
                return img
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

        size = tuple(int(_) for _ in self.geometry().split('+')[0].split('x'))
        x = screen_width / 2 - size[0] / 2
        y = screen_height / 2 - size[1] / 2

        self.geometry("+%d+%d" % (x, y))


def launch_gui():
    window = MainWindow()
    window.reload()
    window.mainloop()


if __name__ == '__main__':
    launch_gui()
