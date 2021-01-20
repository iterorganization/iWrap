from iwrap.job_settings import JobSettings
from iwrap.arguments import Argument, DiagnosticInfo, Parameters
from abcd.arguments import ABCDInArguments, ABCDOutArguments

class ABCDActor:
    
    def __init__(self):
        self.job_settings = JobSettings()
        self.parameters = Parameters()
        self.in_arguments = ABCDInArguments()
        self.out_arguments = ABCDOutArguments()
        print('ABC:ACTOR: Created')

 
  
	 # # #  Actor lifecycle methods # # #
    def initialize(self):
        print('ABC:ACTOR:: INITIALIZE')
        pass

    def run(self, arguments):
        print('ABC:ACTOR:: RUN')
        self.out_arguments.out_arg1.value = 'fake_ids_out1'
        self.out_arguments.out_arg2.value = 'fake_ids_out2'
 
        return self.out_arguments
 

    
    def finalize(self):
        print('ABC:ACTOR:: FINALIZE')
        pass
