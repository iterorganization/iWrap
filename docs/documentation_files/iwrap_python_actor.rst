iWrap Python Actor
==================

Use cases
---------

Wrapper modes:
~~~~~~~~~~~~~~

* interactive - user code is run directly from wrapper
* batch - user executable is sent to a queue

Execution modes
~~~~~~~~~~~~~~~

User code can be run in mode:
*****************************

* Sequential
* Parallel:
    * MPI
        * MPICH
        * OpenMPI
    * and/or OpenMP

Debugging modes:
****************

* standalone - debugger launches executable containing user code in a separate process
* attach - debugger attaches to running process executing user code