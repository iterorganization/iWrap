import tkinter as tk
from tkinter import ttk

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

        self.columnconfigure(1, weight=3)

    def update_settings(self):
        pass

    def reload(self):
        self.actor_name.delete( 0, tk.END )
        self.actor_type_combo.current( 0 )
        self.data_type_combo.current( 0 )
