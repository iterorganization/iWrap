import tkinter as tk
from tkinter import ttk
import importlib
from importlib import resources

from iwrap.gui.generics import IWrapPane


class ActorDescriptionPane(ttk.LabelFrame, IWrapPane):

    def __init__(self, master: ttk.Widget):
        super().__init__(master, text='Actor:', borderwidth=2, relief="groove", height = 100)

        self.grid( column=0, row=0, sticky=(tk.N, tk.S, tk.E, tk.W) )
        ttk.Label( self, text="Name:" ).grid( column=0, row=0, padx=10, pady=5, sticky=tk.W )
        self.actor_name = ttk.Entry(self)
        self.actor_name.grid( column=1, columnspan=10, row=0, padx=10, pady=5, sticky = ( tk.W, tk.E))

        ttk.Label( self, text="Type :").grid( column=0, row=1, padx=10, pady=5, sticky=tk.W )
        actor_types =  ['Python actor', 'Kepler actor']
        self.actor_type_combo = ttk.Combobox( self, width=15, values = actor_types, state='readonly' )
        self.actor_type_combo.grid( column=1, columnspan=10, row=1, padx=10, pady=5, sticky = (tk.E, tk.W) )
        self.actor_type_combo.current( 0 )

        ttk.Label( self, text="Data type :" ).grid( column=0, row=2, padx=10, pady=5, sticky=tk.W )

        data_types = ['IDS', 'HDC']
        self.data_type_combo = ttk.Combobox( self, width=15, values=data_types, state='readonly' )
        self.data_type_combo.grid( column=1, columnspan=10, row=2, padx=10, pady=5, sticky=(tk.E, tk.W) )
        self.data_type_combo.current( 0 )

        # Load IMAS logo next to the ActorDescriptionPane
        self.img: tk.PhotoImage = tk.PhotoImage()
        self.__load_logo()
        # Put logo as label into ActorDescriptionPane grid
        ttk.Label(self, image=self.img).grid(column=11, row=0, rowspan=3, pady=5, padx=(0, 5))

        self.columnconfigure(1, weight=3)

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
        pass

    def reload(self):
        self.actor_name.delete( 0, tk.END )
        self.actor_type_combo.current( 0 )
        self.data_type_combo.current( 0 )
