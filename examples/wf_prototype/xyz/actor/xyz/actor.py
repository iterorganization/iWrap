from iwrap.job_settings import JobSettings
from iwrap.arguments import Argument, DiagnosticInfo, Parameters
from xyz.arguments import XYZInArguments, XYZOutArguments

class XYZActor:
    
    def __init__(self):
        self.job_settings = JobSettings()
        self.parameters = Parameters()
        self.in_arguments = XYZInArguments()
        self.out_arguments = XYZOutArguments()
        print('XYZ:ACTOR: Created')

 
  
	 # # #  Actor lifecycle methods # # #
    def initialize(self):
        print('XYZ:ACTOR:: INITIALIZE')
        pass

    def run(self, arguments):
        print('XYZ:ACTOR:: RUN')
        self.out_arguments.out_arg1.value = 'fake_xyz_ids_out1'
        self.out_arguments.out_arg2.value = 'fake_xyz_ids_out2'
 
        return self.out_arguments
 

    
    def finalize(self):
        print('XYZ:ACTOR:: FINALIZE')
        pass
