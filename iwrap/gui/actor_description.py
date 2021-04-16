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

        # Put IMAS logo next to the ActorDescriptionPane
        #imageFrame= ttk.Frame(self)
        canvas = tk.Canvas(self,width=100, height=100, bd=0, bg=None)
        canvas.grid(column=11,row=0,rowspan=3, sticky=(tk.E, tk.W, tk.N, tk.S))
        self.img=None
        self.__load()
        canvas.create_image((50, 50), anchor=tk.CENTER, image=self.img)
        self.columnconfigure( 1, weight=3 )

    def __load(self):
        package = "iwrap.resources"
        resource = "imas_transparent_logo_small.gif"
        # Try to access icon path using importlib.resource module
        try:
            print('sa')
            with importlib.resources.path(package, resource) as icon_path:
                print('adsg')
                i= tk.PhotoImage(file=icon_path)
                self.img=i
        # Rise Import Error
        except ImportError:
            pass

    def update_settings(self):
        pass

    def reload(self):
        self.actor_name.delete( 0, tk.END )
        self.actor_type_combo.current( 0 )
        self.data_type_combo.current( 0 )
