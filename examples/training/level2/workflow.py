import imas,os

from physics_ii.actor import PhysicsIIActor



class ExampleWorkflowManager:
    
    def __init__(self):
        self.actor_physics_ii = PhysicsIIActor()
        self.input_entry = None
        self.output_entry = None

    def init_workflow(self):
        print( '>> Querying input arguments <<' )
        for arg in self.actor_physics_ii.in_arguments:
            print(arg)

        print( '>> Querying out arguments <<' )
        for arg in self.actor_physics_ii.out_arguments:
            print( arg )


        # INPUT/OUTPUT CONFIGURATION
        shot                = 131024
        run_in              = 1
        input_user_or_path  = 'public'
        input_database      = 'iter'
        run_out             = 10
        output_user_or_path = os.getenv('USER')
        output_database     = input_database

        # OPEN INPUT DATAFILE TO GET DATA FROM IMAS SCENARIO DATABASE
        print('=> Open input datafile')
        self.input_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,input_database,shot,run_in,input_user_or_path)
        self.input_entry.open()
        
        # CREATE OUTPUT DATAFILE
        print('=> Create output datafile')
        self.output_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,output_database,shot,run_out,output_user_or_path)
        self.output_entry.create()

        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        self.actor_physics_ii.initialize() 
    
    def execute_workflow(self):
        # READ INPUT IDSS FROM LOCAL DATABASE
        time_slice          = 200.
        print('=> Read input IDSs')
        input_equilibrium = self.input_entry.get_slice('equilibrium', time_slice, 1)

        # EXECUTE PHYSICS CODE
        print('=> Execute physics code')

        output_equilibrium = self.actor_physics_ii(input_equilibrium)
        
        
        # SAVE IDSS INTO OUTPUT FILE
        print('=> Export output IDSs to local database')
        self.output_entry.put(output_equilibrium)
        print('Done exporting.')


    def end_workflow(self):
        
        # Finalize ALL actors 
        self.actor_physics_ii.finalize() 
        
        #other finalizastion actions
        self.input_entry.close()
        self.output_entry.close()



manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()






