############################################################
Fortran code standardisation
############################################################

########################

Code signature
########################

.. code-block:: Fortran

     module <module name>

     !
     !    INIT/MAIN/FINALIZE SUBROUTINE
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
########################

-  Native code should be put within a module
-  Module is used by compiler to check, if code signature
   expected by wrapper is exactly the same as provided.
-  A name of the module could be arbitrary - chosen by code
   developer

Subroutines
########################
-  A user code should be provided as subroutines (and not a functions)
-  A name of subroutines could be arbitrary - chosen by code developer
-  A name of the module could be arbitrary - chosen by code developer
-  Arguments shall be provided in a strict order
-  No INOUT arguments are allowed!

Arguments
########################

*INIT* / *MAIN* / *FINALIZE* subroutines:

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
########################

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


Code packaging
################
A native code written in Fortran should be packed within static Linux library using e.g. ar tool for that purpose.

.. code-block:: console

    ar -cr lib<name>.a <object files *.o list>
    e.g.:
    ar -cr libphysics_ii.a *.o





