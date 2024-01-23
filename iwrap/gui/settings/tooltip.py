import tkinter as tk

class ToolTip(object):
    def __init__(self, widget, tooltip_index=''):
        self.waittime = 500     #miliseconds
        self.wraplength = 450   #pixels
        self.widget = widget

        try:
            self.text = tooltip_dict[tooltip_index]
        except KeyError:
            self.text = ''

        self.widget.bind("<Enter>", self.enter)
        self.widget.bind("<Leave>", self.leave)
        self.widget.bind("<ButtonPress>", self.leave)
        self.id = None
        self.tw = None

    def enter(self, event=None):
        self.schedule()

    def leave(self, event=None):
        self.unschedule()
        self.hidetip()

    def schedule(self):
        self.unschedule()
        self.id = self.widget.after(self.waittime, self.showtip)

    def unschedule(self):
        id = self.id
        self.id = None
        if id:
            self.widget.after_cancel(id)

    def showtip(self, event=None):
        x = y = 0
        x, y, cx, cy = self.widget.bbox("insert")
        x += self.widget.winfo_rootx() + 25
        y += self.widget.winfo_rooty() + 20
        # creates a toplevel window
        self.tw = tk.Toplevel(self.widget)
        # Leaves only the label and removes the app window
        self.tw.wm_overrideredirect(True)
        self.tw.wm_geometry("+%d+%d" % (x, y))
        label = tk.Label(self.tw, text=self.text, justify='left',
                       background="#ffffff", relief='solid', borderwidth=1,
                       wraplength = self.wraplength)
        label.pack(ipadx=1)

    def hidetip(self):
        tw = self.tw
        self.tw= None
        if tw:
            tw.destroy()

tooltip_dict ={
    'actor_name':'The chosen name for the actor (may be identical or different from the name of the physics code). \n\n'
                 'example: my_actor',
    'actor_type':'The type/flavor of the actor to be generated.\n\n'
                 'default: ‘python’',
    'programming_language':'Language of the physics code. One of the predefined values: Fortran, CPP, None (None will create a skeleton actor class in which calls to physics code have to be added manually).',

    'data_dictionary_compliant':'Oldest known version of Data Directory compatible with the physics code.\n\n'
                                'example: 3.39.0',
    'root_dir':'The root directory for ALL relative paths placed in code description. A relative path leading from YAML file location to actor project root dir.\n\n'
               'Default: ‘.‘',
    'data_type':'Data type handled by the physics code.\nvalue: ‘legacy’ (currently only ‘Legacy IDS’ type has been implemented).',
    'code_path':'Path to system library (C, C++, Fortran) , script (Python), etc., containing the physics code, including methods/subroutines to be run. Relative paths start at "Root dir" directory.\n\n'
                'examples:\n'
                'C++/Fortran: ‘/path/to/code/lib/libcode.a’',
    'include_path':'Path to a header file (C, C++), module (Fortran), etc., containing the declaration of physics code methods/subroutines to be run.\n\n'
                'examples:\n'
                   'C++: ‘/path/to/code/include/code.h’\n'
                   'Fortran: ‘/path/to/code/include/code.mod',
    'init':'Name of the method/subroutine in the API of the code. This method is usually used to set up the initial state of the code, however it may contain any arbitrary actions.\n'
           'IMPORTANT! Must correspond to the name of the method in the code.\n\n'
           'example: my_init_method',
    'main':'Name of the method/subroutine in the API of the code. This method is the main compute part of the code (but can correspond to a single step or to the entire computation).\n'
           'IMPORTANT! Must correspond to the name of the method in the code.\n\n'
           'example: my_main_method',
    'finalize':'Name of the method/subroutine in the API of the code. This method is usually used to clean up the code, however it may contain any arbitrary actions.\n'
           'IMPORTANT! Must correspond to the name of the method in the code.\n\n'
           'example: my_finalize_method',
    'get_status':'Name of the method/subroutine in the API of the code. This method is used to get the current state of the code (e.g. for a checkpoint).\n'
           'IMPORTANT! Must correspond to the name of the method in the code.\n\n'
           'example: my_get_status_method',
    'set_status':'Name of the method/subroutine in the API of the code. This method is used to set a new current state of the code (e.g. for a restart).\n'
           'IMPORTANT! Must correspond to the name of the method in the code.\n\n'
           'example: my_set_status_method',
    'get_timestamp': 'Name of the method/subroutine in the API of the code. This method is used to get the current timestamp (physics time) from the code.\n'
           'IMPORTANT! Must correspond to the name of the method in the code.\n\n'
           'example: my_get_timestamp_method',
    'install_dir':'Path to the directory where the actor will be installed.',
    'documentation':'Human readable description of the actor.',
    'parameters_file':'Path to the parameters (XML file) of the code.\n\n'
                      'example: ‘./code_parameters/parameters.xml’',
    'schema_file':'Path to the schema (XSD file) used to validate the XML code parameters.\n\n'
                  'example: ‘./code_parameters/parameters.xsd’',
    'argument_window.Name':'User defined argument name. Will be used in the actor only.\n\n'
                           'example: ’equilibrium00’',
    'argument_window.Intent':'Determines if the given argument is an input or an output one.',
    'argument_window.Type':'The type of the IDS structure (i.e. the IDS name) associated with this argument.',
    'compiler_cmd':'The compiler command used to compile the code and which will be used to compile the wrapper.\n\n'
                               'examples:\n'
                               '‘gfortran‘, ‘ifort‘, ‘g++‘',
    'open_mp_switch':'A compiler switch/flag to be used if the code uses OpenMP.\n'
                     'examples:\n'
                     ' ‘-fopenmp’, ‘-qopenmp’',
    'mpi_compiler_cmd':'The MPI compiler command used to compile the code and which will be used to compile the wrappper.\n'
                       'IMPORTANT! The existence (or absence) of this entry, determines if the physics code uses MPI (or not).\n\n'
                       'examples:\n'
                       '‘mpif90’, ‘mpiifort’'
}
