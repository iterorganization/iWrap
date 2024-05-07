.. _code_api:

############################################################
Code standardisation
############################################################

Introduction
############

.. warning::
      The signature of the code provided by the user must
      follow strict rules - without the details on method's
      signature iWrap cannot generate an actor.

iWrap actors can call the following methods from the code:

- Basic methods:

  -  *INIT* - Initialisation method
  -  *MAIN* - Mandatory main ("step") method
  -  *FINALIZE* - Finalisation method

- Checkpoint/restart methods

  - *GET_STATE* - Method for getting internal state of the code
  - *SET_STATE* - Method for setting internal state of the code

- Auxiliary methods

  - *GET_TIMESTAMP* - Method for getting currently computed physical time

The name and signatures of each method may differ, depending of
features of programming language being used, however the main
principia remains the same.


Basic methods
################

API
======================
All *INIT*, *MAIN* and *FINALIZE* methods share the same API:

.. image:: /resources/attachments/70877452/70877459.png                                                          |

-  A **mandatory** method in the code where it performs the main computation
-  Can correspond to the entire computation or to a step that can be run an arbitrary number of times (e.g. in a loop)
-  It can be of arbitrary name (the name has to be specified in the code YAML description)
-  The method must be run **after** a call of *initialisation* (if provided) and **before** a call of *finalisation* (if provided)
-  The methods can be of arbitrary names (they have to be specified in the code YAML description)
-  Method arguments:

   -  Input and output IDSes:

      -  **Mandatory**  arguments
      -  Type: An object of IDS class (depends on particular language)
      -  Intent: IN or OUT

   -  XML parameters:

      -  **Optional**  argument
      -  Type: string
      -  Intent: IN



.. warning::
      -  Be aware that generators of some actor types may put additional restrictions on the methods API!
      -  A mechanism of exceptions is used for some languages (Java, Python) to communicate code state,
         instead of output `status code` and `status message` arguments

Methods purpose and usage
==========================

- *INIT* - An optional method used for the initialization/configuration of the code.
  If provided - the method should be called only when an actor is initialised.
  **before** a call of *main* and *finalisation* (if provided).

- *MAIN* - A **mandatory** method in the code where it performs the main computation.
  It can correspond to the entire computation or to a step that can be run an arbitrary number of times (e.g. in a loop)
  The method must be run **after** a call of *initialisation* (if provided) and **before** a call of *finalisation* (if provided)

- *FINALZE* - An optional method that is usually called to clean-up environment and/or to post-process data

Code restarting methods
################
``GET_STATE`` and  ``SET_STATE`` methods enable to restart stateful, sometimes compute demanding,
codes without losing results obtained before computations were stopped. The actor ask periodically
the code about its internal state using the ``GET_STATE`` method. After a restart, the code state
can be restored using the ``SET_STATE`` method.

An internal state of the code has to be passed as a string, however iWrap gives a full flexibility
to the code developer concerning format and content of state description.
It is a kind of a ‘black box’ returned from ``GET_STATE`` and passed to ``SET_STATE`` method during restart,
so the only requirement is that information returned by ``GET_STATE`` is understandable to ``SET_STATE``.

*GET_STATE* method*FINALIZE subroutine:*

-  No INPUT/OUTPUT arguments:
======================

- An optional method used for getting the internal state of the code
- The method must be run **after** a call of ``INIT`` (if provided)
- The method can be of arbitrary name (the name has to be specified in the code YAML description)
- Method arguments:

  - Code state:

    -  **Mandatory** argument
    -  Type: string
    -  Intent: OUT


*SET_STATE* method
======================

- An optional method used for restoring the internal state of the code
- The method must be run **after** a call of ``INIT`` (if provided)
- The method can be of arbitrary name (the name has to be specified in the code YAML description)
- Method arguments:

  - Code state:

    -  **Mandatory** argument
    -  Type: string
    -  Intent: IN


.. warning::
       Important!
          A code wrapped by iWrap that will become a part of workflow should be compiled using the same
          environment in which workflow will be run!


Auxiliary methods
################


*GET_TIMESTAMP* method
======================

- An optional method used for getting currently computed physical time point
- The method must be run **after** a call of ``INIT`` (if provided)
- The method can be of arbitrary name (the name has to be specified in the code YAML description)
- Method arguments:

  - Timestamp:

    -  **Mandatory** argument
    -  Type: double float
    -  Intent: OUT


Error and status reporting
################
The wrapped code can communicate with a caller by throwing exceptions (Java)
or using two **mandatory** output arguments (C++ and Fortran):

  - Status code:

    -  **Mandatory** argument
    -  Type: Integer
    -  Intent: OUT
    -  Values:

       - negative number - *ERROR*
       - 0 - *SUCCESS*
       - positive number - *WARNING*

  - Status message

    -  **Mandatory** argument
    -  Type: string
    -  Intent: OUT

MPI
################
All codes that use MPI should follow the rules described below:

-  Do not call MPI_Init and MPI_Finalize in the code's API, or add such conditional checks before:

    Fortran

    .. code-block:: fortran

      Example code
        !   ----  MPI initialisation ----
        call MPI_initiazed(was_mpi_initialized, ierr)
        if (.not. was_mpi_initialized)   call MPI_Init(ierr)

        !   ----  MPI Finalisation ----
        call MPI_finalized(was_mpi_finalized, ierr)
        if (.not. was_mpi_finalized)   call MPI_Finalize(ierr)

    C++

    .. code-block:: cpp

        int was_mpi_initialized, was_mpi_finalized;

        //----  MPI initialisation ----
        MPI_Initialized(&was_mpi_initialized);
        if (!was_mpi_initialized)
            MPI_Init(NULL, NULL);

        //----  MPI Finalization ----
        MPI_Finalized(&was_mpi_finalized);
        if (!was_mpi_finalized)
           MPI_Finalize();


-  Please be aware of a special role of the process 'rank 0': the wrapper that run the code, launched in parallel,
   reads input data in every processes but writes it only in 'rank 0' process. So the code shall gather in 'rank 0'
   process all results that need to be stored as output.

.. warning::
      iWrap supports only **sequential** Java code!


API implementation
#######################


.. toctree::
   :numbered:
   :maxdepth: 10

   /resources/code_standardization/code_standardization_fortran.rst
   /resources/code_standardization/code_standardization_cpp.rst
   /resources/code_standardization/code_standardization_java.rst



