import tkinter as tk
from tkinter import ttk

from iwrap.gui.generics import IWrapPane


class ActorDescriptionPane(ttk.LabelFrame, IWrapPane):

    def __init__(self, master: ttk.Widget):
        super().__init__(master, text='Actor:', borderwidth=2, relief="groove", height = 100)

        self.grid( column=0, row=0, sticky=(tk.N, tk.S, tk.E, tk.W) )
        ttk.Label( self, text="Name:" ).grid( column=0, row=0, padx=10, pady=5, sticky=tk.W )
        ttk.Entry(self).grid( column=1, columnspan=10, row=0, padx=10, pady=5, sticky = ( tk.W, tk.E))

        ttk.Label( self, text="Type :").grid( column=0, row=1, padx=10, pady=5, sticky=tk.W )
        actor_types =  ['Python actor', 'Kepler actor']
        actor_type_combo = ttk.Combobox( self, width=15, values = actor_types, state='readonly' )
        actor_type_combo.grid( column=1, columnspan=10, row=1, padx=10, pady=5, sticky = (tk.E, tk.W) )
        actor_type_combo.current( 0 )

        ttk.Label( self, text="Data type :" ).grid( column=0, row=2, padx=10, pady=5, sticky=tk.W )

        data_types = ['IDS', 'HDC']
        data__type_combo = ttk.Combobox( self, width=15, values=data_types, state='readonly' )
        data__type_combo.grid( column=1, columnspan=10, row=2, padx=10, pady=5, sticky=(tk.E, tk.W) )
        data__type_combo.current( 0 )

        self.columnconfigure( 1, weight=3 )

    def reload(self):
        pass
