import tkinter as tk

class ToolTip(object):
    def __init__(self, widget, tooltip_index=''):
        self.waittime = 500     #miliseconds
        self.wraplength = 300   #pixels
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
    'actor_name':'The arbitrary, user defined name of the actor. It determines e.g. : the name of class to be generated and directory where actor will be put.',
    'actor_type':'Value: ‘python’ (currently only python type has been implemented)',
    'programming_language':'Language of physics code. One of predefined values: Fortran, CPP.',
    'data_dictionary_compliant':'Oldest known version of Data Directory compatible with actor.',
    'root_dir':'The root directory for ALL relative paths placed in code description. A relative path leading from YAML file location to actor project root dir.\n\n'
               'Default: ‘.‘',
    'data_type':'Data type handled by the physics code. value: ‘legacy’ (currently only ‘Legacy IDS’ type has been implemented).',
    'code_path':'Path to system library (C, C++, Fortran) , script (Python), etc., containing the physics code, including methods/subroutines to be run. Relative paths start at "Root dir" directory.\n\n'
                'examples:\n'
                'C++/Fortran: ‘/path/to/code/lib/libcode.a’',
    'include_path':'Path to a header file (C, C++), module (Fortran), etc., containing the declaration of physics code methods/subroutines to be run.\n\n'
                'examples:\n'
                   'C++: ‘/path/to/code/include/code.h’\n'
                   'Fortran: ‘/path/to/code/include/code.mod',
    'init':'Name of user method / subroutine to be called. It is used, usually, to set up the native code, however subroutine may contain any arbitrary actions.',
    'main':'Name of user method / subroutine to be called. It is main method of your physics code.',
    'finalize':'Name of user method / subroutine to be called. It is used, usually, to clean up the native code, however subroutine may contain any arbitrary actions.',
    'get_status':'Name of user method / subroutine to be called. It is used, usually, to get current state of native code, however subroutine may contain any arbitrary actions.',
    'set_status':'Name of user method / subroutine to be called. It is used, usually, to set current state of native code, however subroutine may contain any arbitrary actions.',
    'get_timestamp': 'Name of user method / subroutine to be called. It is used, usually, to get timestamp of native code, however subroutine may contain any arbitrary actions.',
    'parameters_path':'Path to XML file containing user defined parameters of the physics model.\n\n'
       'example: ‘./code_parameters/parameters.xml’',
    'schema_path':'Path to XSD file contains schema of XML parameters, enabling its validation.',
    'install_dir':'Path to directory where actor folder will be created.',
    'documentation':'Human readable description of native code.',
    'parameters_file':'Path to XML file containing user defined parameters of the physics model.\n\n'
                      'example: ‘./code_parameters/parameters.xml’',
    'schema_file':'Path to XSD file contains schema of XML parameters, enabling its validation.\n\n'
                  'example: ‘./code_parameters/parameters.xsd’',
    'argument_window.Name':'User defined argument name. Can be used in actor\'s main method call.\n\n'
                           'example: ’equilibrium00’\n'
                           'example argument\'s name usage: ’result = my_actor(equilibrium00=some_ids_variable)’',
    'argument_window.Input':'Determines if given argument is input or output one.',
    'argument_window.Type':'A type of an IDS argument.',
    'compiler_cmd':'Name/vendor of the compiler command used to compile native codes.\n\n'
                               'examples:\n'
                               '‘gfortran‘, ‘ifort‘, ‘g++‘',
    'open_mp_switch':'A compiler switch to be used if native code use OpenMP.\n'
                     'examples:\n'
                     ' ‘-fopenmp’, ‘-qopenmp’',
    'mpi_compiler_cmd':'The name/vendor of the MPI compiler command used to compile native codes.\n'
                       'IMPORTANT! The existence (or absence) of this entry, determines if native codes use MPI or not.\n\n'
                       'examples:\n'
                       '‘mpif90’, ‘ifort’'
}
tooltip_dict['argument_window.Output'] = tooltip_dict['argument_window.Input']