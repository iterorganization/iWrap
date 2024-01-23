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

*INIT* method
======================

.. image:: /resources/attachments/70877452/77370373.png


- An optional method used for the initialization/configuration of the code
- If provided - the method is called only when an actor is initialized
- The method must be run **before** a call of *main* and *finalisation* (if provided)
- The method can be of arbitrary name (the name has to be specified in the code YAML description)
- Method arguments:

  - Code parameters:

    -  **Optional** argument
    -  Type: string
    -  Intent: IN
  - Status code:

    -  **Mandatory** argument
    -  Type: Integer
    -  Intent: OUT
  - Status message

    -  **Mandatory** argument
    -  Type: string
    -  Intent: OUT

*MAIN* method
======================

.. image:: /resources/attachments/70877452/70877459.png                                                          |

-  A **mandatory** method in the code where it performs the main computation
-  Can correspond to the entire computation or to a step that can be run an arbitrary number of times (e.g. in a loop)
-  It can be of arbitrary name (the name has to be specified in the code YAML description)
-  The method must be run **after** a call of *initialisation* (if provided) and **before** a call of *finalisation* (if provided)
-  Method arguments:

   -  Input and output IDSes:

      -  **Optional**\  arguments
      -  Intent: IN or OUT

   -  XML parameters:

      -  **Optional**  argument
      -  Type: string
      -  Intent: IN

   -  Status code:

      -  **Mandatory**\  argument
      -  Type: Integer
      -  Intent: OUT

   -  Status message

      -  **Mandatory** argument
      -  Type: string
      -  Intent: OUT

*FINALIZE* method
======================
   .. image:: attachments/70877452/77370389.png

-  An optional method that is usually called to clean-up environment
-  The method can be run an arbitrary numer of times
-  The method can be of arbitrary name (the name has to be specified in the code YAML description)
-  Method arguments:

   -  Status code:

      -  **Mandatory**\  argument
      -  Type: Integer
      -  Intent: OUT

   -  Status message

      -  **Mandatory**\  argument
      -  Type: string
      -  Intent: OUT

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

*GET_STATE* method
======================

- An optional method used for getting the internal state of the code
- The method must be run **after** a call of ``INIT`` (if provided)
- The method can be of arbitrary name (the name has to be specified in the code YAML description)
- Method arguments:

  - Code state:

    -  **Mandatory** argument
    -  Type: string
    -  Intent: OUT
  - Status code:

    -  **Mandatory** argument
    -  Type: Integer
    -  Intent: OUT
  - Status message

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
  - Status code:

    -  **Mandatory** argument
    -  Type: Integer
    -  Intent: OUT
  - Status message

    -  **Mandatory** argument
    -  Type: string
    -  Intent: OUT


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
  - Status code:

    -  **Mandatory** argument
    -  Type: Integer
    -  Intent: OUT
  - Status message

    -  **Mandatory** argument
    -  Type: string
    -  Intent: OUT

API implementation
#######################

Fortran
======================

Code's API signature
-----------------------

.. code-block:: Fortran

     module <module name>

     !
     !    INITIALISATION SUBROUTINE
     !
     subroutine <init subroutine name> ([xml_parameters,] status_code, status_message)
       use ids_schemas

       ! XML code parameters
       type(ids_parameters_input) :: xml_parameters

       ! status info
       integer, intent(OUT) :: status_code
       character(len=:), pointer, intent(OUT) :: status_message

     end subroutine <init subroutine name>

     !
     !    MAIN SUBROUTINE
     !
     subroutine <subroutine name> ([ids1, ids2, ..., idsN,] [xml_parameters], status_code, status_message)
       use ids_schemas
       ! IN/OUT IDSes
       type(ids_<ids_name>), intent([IN|OUT]):: ids1
       type(ids_<ids_name>), intent([IN|OUT]):: ids2
        . . .
       type(ids_<ids_name>), intent([IN|OUT]):: idsN

       ! XML code parameters
       type(ids_parameters_input) :: xml_parameters

       ! status info
       integer, intent(OUT) :: status_code
       character(len=:), pointer, intent(OUT) :: status_message

     end subroutine <subroutine name>

     !
     !    FINALISATION SUBROUTINE
     !
     subroutine <finish subroutine name> (status_code, status_message)
       use ids_schemas

       ! status info
       integer, intent(OUT) :: status_code
       character(len=:), pointer, intent(OUT) :: status_message

     end subroutine <finish subroutine name>

    !
    !    GET_STATE SUBROUTINE
    !
    subroutine <get_state subroutine name> (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(out) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


    end subroutine <get_state subroutine name>


    !
    !    SET_STATE SUBROUTINE
    !
    subroutine <set_state subroutine name> (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(in) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

    end subroutine <set_state subroutine name>

    !
    !    GET_TIMESTAMP SUBROUTINE
    !
    subroutine <get_timestamp subroutine name>(timestamp_out, status_code, status_message)

        real(8), intent(out) :: timestamp_out
        !----  Status info  ----
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

    end subroutine <get_timestamp subroutine name>

    end module <module name>


Module
-----------------------

-  Subroutines of the code shall be put within a module
-  The module is used by compilers to check if the validity of the signatures
   when compiling the wrappers
-  The name of the module can be arbitrary chosen in the code

Subroutines
-----------------------
-  The code API shall be provided as subroutines (and not as functions)
-  The name of the subroutines can be arbitrary chosen in the code 
-  Arguments of each subroutine shall be provided in a strict order
-  No INOUT arguments are allowed!

Arguments
-----------------------

*INIT subroutine:*

-  XML parameters:

   -  **Optional**  argument
   -  Intent: IN
   -  Defined as ``type(ids_parameters_input), intent(IN)``

-  Status code:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as  ``integer, intent(OUT)``

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as: ``character(len=:), pointer, intent(OUT)``

*MAIN subroutine:*

-  Input and output IDSes:

   -  **Optional** arguments
   -  Intent: IN or OUT
   -  Defined as ``type(ids_<ids_name>)``

-  XML parameters:

   -  **Optional** argument
   -  Intent: IN
   -  Defined as ``type(ids_parameters_input), intent(IN)``

-  Status code:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as  ``integer, intent(OUT)``

-  Status message

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as: ``character(len=:), pointer, intent(OUT)``

*FINALIZE subroutine:*

-  Status code:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as  ``integer, intent(OUT)``

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as: ``character(len=:), pointer, intent(OUT)``


*GET_STATE subroutine:*

-  Code state:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as ``character(len=:), allocatable, intent(OUT)``

-  Status code:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as  ``integer, intent(OUT)``

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as: ``character(len=:), pointer, intent(OUT)``


*SET_STATE subroutine:*

-  Code state:

   -  **Mandatory**  argument
   -  Intent: IN
   -  Defined as ``character(len=:), allocatable, intent(IN)``

-  Status code:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as  ``integer, intent(OUT)``

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as: ``character(len=:), pointer, intent(OUT)``

*GET_TIMESTAMP subroutine:*

-  Timestamp:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as ``real(8), intent(OUT)``

-  Status code:

   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as  ``integer, intent(OUT)``

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as: ``character(len=:), pointer, intent(OUT)``


.. warning::
   Only XML parameters are passed to the code, so only ``parameters_value`` field
   of ``ids_parameters_input`` derived type is valid !

Example
-----------------------

.. code-block:: fortran

     module physics_ii_mod

         !
         !    INITIALISATION SUBROUTINE
         !
     subroutine init_code (xml_parameters, status_code, status_message)
         use ids_schemas, only: ids_parameters_input
         implicit none
         type(ids_parameters_input) :: xml_parameters
         integer, intent(out) :: status_code
         character(len=:), pointer, intent(out) :: status_message

         ! Setting status to SUCCESS
         status_code = 0
         allocate(character(50):: status_message)
         status_message = 'OK'

         write(*,*) '============ The subroutine body ============='

     end subroutine init_code

         !
         !    MAIN SUBROUTINE
         !

     subroutine physics_ii(equilibrium_in, equilibrium_out, code_param, error_flag, error_message)

       use ids_schemas

       ! IN/OUT IDSes
       type(ids_equilibrium):: equilibrium_in, equilibrium_out

       ! XML code parameters
       type(ids_parameters_input) :: code_param

       ! status info
       integer, intent(out) :: error_flag
       character(len=:), pointer, intent(out) :: error_message

     end subroutine physics_ii

         !
         !    FINALISATION SUBROUTINE
         !
     subroutine clean_up(status_code, status_message)
         implicit none
         integer, intent(out) :: status_code
         character(len=:), pointer, intent(out) :: status_message

         ! Setting status to SUCCESS
         status_code = 0
         allocate(character(50):: status_message)
         status_message = 'OK'

         write(*,*) '============ The subroutine body ============='

     end subroutine clean_up

    !
    !    GET_STATE SUBROUTINE
    !
    subroutine get_code_state (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(out) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

         write(*,*) '============ The subroutine body ============='

    end subroutine get_code_state


    !
    !    SET_STATE SUBROUTINE
    !
    subroutine restore_code_state (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(in) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        write(*,*) '============ The subroutine body ============='

    end subroutine restore_code_state

    !
    !    GET TIMESTAMP SUBROUTINE
    !
    subroutine get_timestamp(timestamp_out, status_code, status_message)

        real(8), intent(out) :: timestamp_out
        !----  Status info  ----
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        write(*,*) '============ The subroutine body ============='

    end subroutine get_timestamp


    end module physics_ii_mod

C++
======================


Code signature
-----------------------

.. code-block:: cpp

     #include "UALClasses.h"

     /* * * INIT method * * */
     void <method name>([IdsNs::codeparam_t codeparam,] int& status_code, std::string& status_message)

     /* * * MAIN method * * */
     void <method name>([IdsNs::IDS::<ids_name>& ids1, ..., IdsNs::IDS::<ids_name>& idsN,] [IdsNs::codeparam_t codeparam,] int& status_code, std::string& status_message)

     /* * * FINALIZE method * * */
     void <method name>(int& status_code, std::string& status_message)

     /* * * GET_STATE method * * */
    void <method name>( std::string& state_out, int& status_code, std::string& status_message);

     /* * * SET_STATE method * * */
    void <method name>( std::string state, int& status_code, std::string& status_message);

void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message);

Header
-----------------------

To generate an actor user has to provide a header file containing
the signature of each method of the code's API. This header file
name and name of methods can be chosen arbitrary in the code.


Method
-----------------------

-  The code API shall be provided as methods (and not as functions)
-  The name of methods can be arbitrary chosen in the code  
-  The arguments shall be provided in a strict order
-  No INOUT arguments are allowed!

Arguments
-----------------------

*INIT subroutine:*

-  XML parameters:

   -  **Optional**  argument
   -  Input argument
   -  Defined as   ``IdsNs::codeparam_t``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**\  argument
   -  Output argument
   -  Defined as: ``std::string&``

*MAIN subroutine:*

-  Input and output IDSes:

   -  **Optional** arguments
   -  Input or output argument
   -  Defined as ``const IdsNs::IDS::<ids_name>`` (input) or ``IdsNs::IDS::<ids_name>&`` (output)

-  XML parameters:

   -  **Optional** argument
   -  Input argument
   -  Defined as   ``IdsNs::codeparam_t``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

*FINALIZE subroutine:*

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``


*GET_STATE subroutine:*

-  Code state:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``


*SET_STATE subroutine:*

-  Code state:

   -  **Mandatory**  argument
   -  Input argument
   -  Defined as: ``std::string``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

*GET_TIMESTAMP subroutine:*

-  Timestamp:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``double&``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

No INOUT arguments are allowed!


.. warning::
   Only XML parameters are passed to the code, so only ``parameters`` field
   of ``IdsNs::codeparam_t`` structure type is valid !

Example
-----------------------

**Header file - physics_ii.h**

.. code-block:: cpp

     #ifndef _LEVEL_II_CPP
     #define _LEVEL_II_CPP

     #include "UALClasses.h"

     /* * *   INITIALISATION method   * * */
     void init_code (IdsNs::codeparam_t codeparam, int& status_code, std::string& status_message);

     /* * *   MAIN method   * * */
     void physics_ii_cpp(const IdsNs::IDS::equilibrium& in_equilibrium,
                               IdsNs::IDS::equilibrium& out_equilibrium,
                               IdsNs::codeparam_t codeparam,
                               int& status_code, std::string& status_message);

     /* * *   FINALISATION method   * * */
     void clean_up(int& status_code, std::string& status_message);

     /* * * GET_STATE method * * */
    void get_code_state( std::string& state_out, int& status_code, std::string& status_message);

     /* * * SET_STATE method * * */
    void restore_code_state( std::string state, int& status_code, std::string& status_message);

     /* * * GET_TIMESTAMP method * * */
    void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message);

     #endif // _LEVEL_II_CPP

**Implementation file - level_ii.cpp**

.. code-block:: cpp

     #include "UALClasses.h"

     /* * *   INITIALISATION method   * * */
     void init_code (IdsNs::codeparam_t codeparam, int& status_code, std::string& status_message)
     {
     ...
     // method body
     ...
     }

     /* * *   MAIN method   * * */
     void physics_ii_cpp(const IdsNs::IDS::equilibrium& in_equilibrium,
                               IdsNs::IDS::equilibrium& out_equilibrium,
                               IdsNs::codeparam_t codeparam,
                               int& status_code, std::string& status_message)
     {
     ...
     // method body
     ...
     }

     /* * *   FINALISATION method   * * */
     void clean_up(int& status_code, std::string& status_message)
     {
     ...
     // method body
     ...
     }

     /* * * GET_STATE method * * */
    void get_code_state( std::string& state_out, int& status_code, std::string& status_message)
    {
         ...
         // method body
         ...
    }

     /* * * SET_STATE method * * */
    void restore_code_state( std::string state, int& status_code, std::string& status_message)
    {
         ...
         // method body
         ...
    }

     /* * * GET_TIMESTAMP method * * */
    void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message)
    {
         ...
         // method body
         ...
    }

MPI
################
All codes that use MPI should follow the rules described below:

-  Do not call MPI_Init and MPI_Finalize in the code's API, or add such conditional checks before:

    .. code-block:: fortran

      Example code
        !   ----  MPI initialisation ----
        call MPI_initiazed(was_mpi_initialized, ierr)
        if (.not. was_mpi_initialized)   call MPI_Init(ierr)

        !   ----  MPI Finalisation ----
        call MPI_finalized(was_mpi_finalized, ierr)
        if (.not. was_mpi_finalized)   call MPI_Finalize(ierr)

-  Please be aware of a special role of the process 'rank 0': the wrapper that run the code, launched in parallel,
   reads input data in every processes but writes it only in 'rank 0' process. So the code shall gather in 'rank 0'
   process all results that need to be stored as output. 


Code packaging
################
A code written in C++ or Fortran should be packed in a static Linux library. E.g. using the 'ar' tool:

.. code-block:: console

    ar -cr lib<name>.a <object files *.o list>
    e.g.:
    ar -cr libphysics_ii.a *.o





