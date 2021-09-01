iWrap generated actors
======================

.. figure:: /resources/iwrap_example_of_layers.png
   :align: center

   *Actor general layout*

.. figure:: /resources/actor_architecture.png
   :align: center

   *Actor architecture*

Actor API
---------

* Actor API allows for bidirectional communication between workflow system and actor
* Actor API strictly depends on targeted workflow system that determines:
    * How data are passed to actor / get from an actor
    * How an orchestrator "launches" particular actions on actor
* Actor 'lifecycle': i.e. sequence of method called by workflow system (e.g. *initialise*, *fire*, *finish*)

Wrapper API / Languages binding
-------------------------------

* Languages binding can be used to wrap shared libraries in languages such as Python, Java, ...
* Languages binding allows to: calls methods, pass data from one programming language to other one
* Examples of languages binding:
    * Java JNI  (Java ← → C)
    * Python ctypes  (Python ← → C)
    * Cython  (Python ← → C)
    * f2py (Python ← → Fortran)
    * Fortran iso_c_binding (Fortran ← → C)
* Wrapper API reflects to actor API and to native code API
* An API is influenced by chosen language binding mechanism

Wrapper
-------

* A wrapper is written in the same language as native code is implemented
* A wrapper provides an auxiliary functionality, intermediating between actor and user code in terms of:
    * reading/writing of in/out physical data (IDS)
    * passing arguments of standard types to/from the actor
* A wrapper calls native code methods
* Provided as a shared library

Native code API
---------------

* A "signature(s)" of native, wrapped code
* A standardised API, that allows higher levels to call native code

.. figure:: /resources/actor_lifecycle.png
   :align: center

   *Interaction between actor components*

Interaction between actor components
------------------------------------

#. Scenario is initialised
    #. Workflow system creates an actor (Python) object
    #. Workflow system calls initialise  method of an actor
    #. Internal initialisation of an actor is performed:
    #. Control returns to workflow system
#. A workflow system runs scenario, actors are 'fired' keeping defined actor order of the scenario
#. An actor is launched:
    #. Workflow system passes data provided by previous actor to the fired one
    #. Workflow system calls run method on actor
        #. All IDSes passed as objects to actor are saved in memory (e.g. memory backend of AL)
        #. An actor converts data using chosen languages binding
        #. An actor calls method of the wrapper
    #. Control is passed to wrapper
#. Wrapper method is executed
    #. Wrapper reads IDSes, saved by actor (reading/writing IDSes is necessary to effectively pass a very complex structure from programming language of the actor to that of the native code)
    #. Additional, auxiliary conversions are performed, from data types forced by languages binding mechanism
        #. Example: Fortran - conversion of strings from character array ( character, dim(:)  required by ISO_C_BINDING module) to Fortran string (. character(:) )
    #. A wrapper call native code from a library provided by user
#. Execution of a native code from user library
#. Wrapper
    #. Additional, auxiliary conversions are performed, TO data types forced by languages binding mechanism
    #. Output IDSes are saved
    #. Control goes back to the actor
#. Actor
    #. Clean up actions, that should be performed at every actor call are taken
    #. Control returns to workflow system
#. Workflow system - scenario continues execution
    #. Workflow orchestrator  runs further elements of scenario
    #. If particular actor is called more than once (e.g. in a loop) it is launched again (p3)
#. Workflow system - end of the scenario
    #. Workflow system runs finalise on every actor, to allow actor to e.g. make a cleanup that should be performed only when workflow ends