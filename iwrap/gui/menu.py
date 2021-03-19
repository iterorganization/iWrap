import tkinter as tk


class MenuBar( tk.Menu ):
    def __init__(self, master: tk.Tk):
        super().__init__( master )

        file_menu = tk.Menu( self, tearoff=0 )
        file_menu.add_command( label='New', command=None )
        file_menu.add_separator()

        import_menu = tk.Menu( file_menu, tearoff=0 )
        import_menu.add_command( label='Import code description...', command=None )
        file_menu.add_cascade( label='Import', menu=import_menu )

        export_menu = tk.Menu( file_menu, tearoff=0 )
        export_menu.add_command( label='Export code description...', command=None )
        file_menu.add_cascade( label='Export', menu=export_menu )

        file_menu.add_separator()
        file_menu.add_command( label='Quit', command=master.winfo_toplevel().destroy )

        self.add_cascade( label='File', menu=file_menu )