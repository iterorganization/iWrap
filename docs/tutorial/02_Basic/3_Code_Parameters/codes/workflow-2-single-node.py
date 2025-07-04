import sys

import imas, os

from actor1_fortran.actor import actor1_fortran
from actor2_cpp.actor import actor2_cpp

# # # # # # # # INITIALIZATION # # # # # # # #

# Reading of input data
db_entry_in = imas.DBEntry(backend_id=imas.ids_defs.MDSPLUS_BACKEND,
                           db_name="tutorial_db",
                           shot=1, run=1)

db_entry_in.open()
ids1 = db_entry_in.get('core_profiles')
db_entry_in.close()

# Creation of actors
actor1_fortran = actor1_fortran()
actor2_cpp = actor2_cpp()

# Code parameters single node for actor1_fortran editing
code_parameters = actor1_fortran.get_code_parameters()
code_parameters.set_parameter('parameters/greeting', "Witaj!" )

# Initialization of actors
actor1_fortran.initialize(code_parameters=code_parameters)
actor2_cpp.initialize()

# # # # # # # # COMPUTATIONS  # # # # # # # #

# Workflow specific computations

for i in range(5):
    print("\n\nITERATION: ", i)
    ids2 = actor1_fortran(ids1)
    ids1 = actor2_cpp(ids2)

# # # # # # # # FINALIZATION  # # # # # # # #
# Finalization of actors
actor1_fortran.finalize()
actor2_cpp.finalize()

print("\n\n--- WORKFLOW  OUTCOME: --- \n")
print(ids1.ids_properties.comment)

# Saving of output data
db_entry_out = imas.DBEntry(backend_id=imas.ids_defs.MDSPLUS_BACKEND,
                            db_name="tutorial_db",
                            shot=2, run=2)
db_entry_out.create()
db_entry_out.put(ids1)
db_entry_out.close()




