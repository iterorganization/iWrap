.. _native_code_api:

############################################################
Native code standardisation
############################################################

Introduction
############

.. warning::
      A signature of user code must follow strict rules to
      be wrapped by iWrap - without the detailed knowledge
      of method signature iWrap cannot built an actor.

iWrap actor calls three methods of the native code:

-  The initialisation method
-  The main ("step") method
-  The finalisation method

Signatures of methods may differ, depending of features of
programming language being used, however the main principia
remains the same.


The method API
################


Initialisation method
======================

.. image:: /resources/attachments/70877452/77370373.png


- An optional method used for set-up of native code
- If provided - the method is called only, when an actor is initialised
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

Main method
======================

.. image:: attachments/70877452/70877459.png                                                          |

-  A **mandatory** method that native code **must** provide
-  The method can be run an arbitrary numer of times (e.g. in a loop)
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

Finalize method
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

.. warning::
       Important!
          A native code wrapped by iWrap that will become a part of workflow should be compiled using the same
          environment in which workflow will be run!

API implementation
#######################

Fortran
======================

Native code signature
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
     end module <module name>


Module
-----------------------

-  Native code should be put within a module
-  Module is used by compiler to check, if code signature
   expected by wrapper is exactly the same as provided.
-  A name of the module could be arbitrary - chosen by code
   developer

Subroutines
-----------------------
-  A user code should be provided as subroutines (and not a functions)
-  A name of subroutines could be arbitrary - chosen by code developer
-  A name of the module could be arbitrary - chosen by code developer
-  Arguments shall be provided in a strict order
-  No INOUT arguments are allowed!

Arguments
-----------------------

*Initialisation subroutine:*

-  XML parameters:

   -  **Optional**\  argument
   -  Intent: IN
   -  Defined as
      "  type(ids_parameters_input), intent(IN)"

-  Status code:

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as  "  integer, intent(OUT)"

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined
      as: \   character(len=:), pointer, intent(OUT)

*Main subroutine:*

-  Input and output IDSes:

   -  **Optional**\  arguments
   -  Intent: IN or OUT
   -  Defined as "  type(ids_<ids_name>)  \  "

-  XML parameters:

   -  **Optional**\  argument
   -  Intent: IN
   -  Defined as
      "  type(ids_parameters_input), intent(IN)"

-  Status code:

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as  "  integer, intent(OUT)"  \

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined
      as: \   character(len=:), pointer, intent(OUT)

*Finalisation subroutine:*

-  Status code:

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined as  "  integer, intent(OUT)"  \

-  Status message

   -  **Mandatory**\  argument
   -  Intent: OUT
   -  Defined
      as: \   character(len=:), pointer, intent(OUT)


.. warning::
   Only XML parameters are passed to native code, so only ``parameters_value`` field
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


     end module physics_ii_mod

C++
======================


Native code signature
-----------------------

.. code-block:: cpp

     #include "UALClasses.h"

     /* * * Initialisation method * * */
     void <method name>([IdsNs::codeparam_t codeparam,] int& status_code, std::string& status_message)

     /* * * Main method * * */
     void <method name>([IdsNs::IDS::<ids_name>& ids1, ..., IdsNs::IDS::<ids_name>& idsN,] [IdsNs::codeparam_t codeparam,] int& status_code, std::string& status_message)

     /* * * Finalisation method * * */
     void <method name>(int& status_code, std::string& status_message)

Header
-----------------------

To generate an actor user has to provide a file containing
C++ header of wrapped method. This file can be of arbitrary
name but must contain method signature.

Method
-----------------------

-  A user code should be provided as methods (and not a functions)
-  A name of methods could be arbitrary - chosen by code developer
-  Arguments shall be provided in a strict order
-  No INOUT arguments are allowed!

Arguments
-----------------------

Arguments shall be provided in a strict order:

-  Input IDSes:

   -  **Optional**\  arguments
   -  Defined as   ``const IdsNs::IDS::<ids_name>&``

-  Output IDSes:

   -  **Optional**\  arguments
   -  Defined as   ``IdsNs::IDS::<ids_name>&``

-  XML parameters:

   -  **Optional**\  argument
   -  Input argument
   -  Defined as   ``IdsNs::codeparam_t``

-  Status code:

   -  **Mandatory**\  argument
   -  Output argument
   -  Defined as    ``int&``

-  Status message

   -  **Mandatory**\  argument
   -  Output argument
   -  Defined as: ``std::string&``

No INOUT arguments are allowed!


.. warning::
   Only XML parameters are passed to native code, so only ``parameters`` field
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

MPI
################
All native codes that use MPI should follow the rules described below:

-  Please make initialisation and finalisation conditional checking if such action was already made.

    .. code-block:: fortran

      Example code
        !   ----  MPI initialisation ----
        call MPI_initiazed(was_mpi_initialized, ierr)
        if (.not. was_mpi_initialized)   call MPI_Init(ierr)

        !   ----  MPI Finalisation ----
        call MPI_finalized(was_mpi_finalized, ierr)
        if (.not. was_mpi_finalized)   call MPI_Finalize(ierr)

-  Please be aware of a special role of ranked 0 process: Wrapper that run native code, launched in parallel,
   reads input data in every processes but writes it only in'rank 0' process. So native code should gather all
   results that need to be stored by 'rank 0' process. It concerns also those coming from 'rank 0' process are
   analysed by wrapper.


Code packaging
################
A native code written in C++ or Fortran should be packed within static Linux library using e.g. ar tool for that purpose.

.. code-block:: console

    ar -cr lib<name>.a <object files *.o list>
    e.g.:
    ar -cr libphysics_ii.a *.o





