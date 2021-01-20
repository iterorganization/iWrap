
class Argument :
    IN = 10
    OUT = 20

    def __init__(self, arg_type, sub_type, intent):
        self.type = arg_type # (INT, DOUBLE, STRING, COMPLEX, IDS) READ ONLY
        self.sub_type = sub_type  #'equilibrium'  for IDSes only READ ONLY
        self.intent = intent # (IN/OUT) READ ONLY
        self.value  = 7. # to be set by user
      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
class DiagnosticInfo :
    def __init__(self):
        self.status = 0
        self.message = ''

class Parameters : 
    def __init__(self):
        self.code_parameters = None # file name or string or... (?)
        self.default_parameters = None # file name or string or ... (?)
        self.schema = None # file name or string or ... (?)
