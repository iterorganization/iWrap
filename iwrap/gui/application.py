import tkinter as tk
from tkinter import ttk

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


class MainWindow( tk.Tk, IWrapPane ):
    def __init__(self):
        super().__init__()

        self.title( "iWrap" )
        self.minsize( 600, 300 )
        self.geometry( '600x600' )

        self.config( menu=MenuBar( self ) )

        main_pane = ttk.Frame( self )
        main_pane.pack( fill=tk.BOTH, expand=True )

        self.actor_description = ActorDescriptionPane( main_pane )
        self.actor_description.pack( fill=tk.BOTH, side=tk.TOP, padx=5, pady=5 )

        button_pane = ButtonPane(main_pane)
        button_pane.pack(fill=tk.X, side=tk.BOTTOM)

        self.settings_pane = SettingsMainPane( main_pane )
        self.settings_pane.pack( fill=tk.BOTH, side=tk.TOP, expand=True )

        self.center()

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


window = MainWindow()

window.mainloop()
