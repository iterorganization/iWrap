import tkinter as tk
from tkinter import ttk

from iwrap.generation_engine.engine import Engine
from iwrap.gui.generics import IWrapPane
from iwrap.settings.project import ProjectSettings


class ActorDescriptionPane( ttk.LabelFrame, IWrapPane ):

    def __init__(self, master: ttk.Widget):
        super().__init__( master, text='Actor:', borderwidth=2, relief="groove", height=100 )

        self.grid( column=0, row=0, sticky=(tk.N, tk.S, tk.E, tk.W) )
        ttk.Label( self, text="Name:" ).grid( column=0, row=0, padx=10, pady=5, sticky=tk.W )
        self.actor_name = ttk.Entry( self )
        self.actor_name.grid( column=1, columnspan=10, row=0, padx=10, pady=5, sticky=(tk.W, tk.E) )

        ttk.Label( self, text="Type :" ).grid( column=0, row=1, padx=10, pady=5, sticky=tk.W )

        self.actor_type_combo = ttk.Combobox( self, width=15, state='readonly' )
        self.actor_type_combo.grid( column=1, columnspan=10, row=1, padx=10, pady=5, sticky=(tk.E, tk.W) )
        self.actor_type_combo.bind( "<<ComboboxSelected>>", self.actor_type_combo_action )

        ttk.Label( self, text="Data type :" ).grid( column=0, row=2, padx=10, pady=5, sticky=tk.W )

        self.data_type_combo = ttk.Combobox( self, width=15, state='readonly' )
        self.data_type_combo.grid( column=1, columnspan=10, row=2, padx=10, pady=5, sticky=(tk.E, tk.W) )

        self.columnconfigure( 1, weight=3 )

    def actor_type_combo_action(self, event):
        selected_index = self.actor_type_combo.current()
        new_generator = Engine().registered_generators[selected_index]
        Engine().active_generator = new_generator

        self.actor_type_combo.selection_clear()

    def update_settings(self):
        # updating actor name
        actor_name = self.actor_name.get()
        ProjectSettings.get_settings().name = actor_name

        # updating actor type
        actor_type = self.actor_type_combo.get()
        ProjectSettings.get_settings().actor_type = actor_type

        # updating actor data type
        data_type = self.data_type_combo.get()
        ProjectSettings.get_settings().data_type = data_type

    def reload(self):
        # set values of actor types combo
        actor_types = Engine().registered_generators
        self.actor_type_combo.configure( values=actor_types )

        # set active generator in combo
        active_generator = Engine().active_generator
        self.actor_type_combo.current( 0 )
        self.actor_type_combo.set( active_generator )

        # set values of actor data type combo
        data_types = active_generator.actor_data_types
        self.data_type_combo.configure( values=data_types )

        # set actor data type in combo
        data_type = ProjectSettings.get_settings().data_type
        if data_type is not None and data_type != '':
            self.data_type_combo.set( data_type )
        else:
            self.data_type_combo.current( 0 )

        self.actor_name.delete( 0, tk.END )
        actor_name = ProjectSettings.get_settings().name
        self.actor_name.insert( 0, actor_name )
