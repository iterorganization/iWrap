module mod_code_lifecycle

contains

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

        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: INITIALISATION called'
        write(*,*) '======================================='

    end subroutine init_code


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

        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: FINALISATION called'
        write(*,*) '======================================='

    end subroutine clean_up

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_lifecycle(equilibrium_in, equilibrium_out, xml_parameters, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid
        use ids_routines, only: ids_copy

        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        type(ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        ! INITIALISATION OF ERROR FLAG
        status_code = 0

        ! INITIAL DISPLAY
        write(*,*) ' '
        write(*,*) '======================================='
        write(*,*) 'START OF PHYSICS CODE'

        ! CHECK IF INPUT IDS IS VALID
        if (ids_is_valid(equilibrium_in%ids_properties%homogeneous_time).eqv..True. &
            .and.size(equilibrium_in%time)>0) then

            ! COPY THE INPUT IDS IN THE OUTPUT IDS
            call ids_copy(equilibrium_in, equilibrium_out)

            ! MANDATORY FLAG (UNIFORM TIME HERE)
            equilibrium_out%ids_properties%homogeneous_time = 1
            allocate(equilibrium_out%code%name(1))   ! For a string of 132 characters max.

            equilibrium_out%code%name(1)   = 'EXAMPLE: code_lifecycle'
            allocate(equilibrium_out%code%version(1))   ! For a string of 132 characters max.

            equilibrium_out%code%version(1)   = '1.0'
            allocate(equilibrium_out%code%parameters(1))   ! For a string of 132 characters max.
            equilibrium_out%code%parameters(1) = 'EXAMPLE: code_lifecycle: NO PARAMETERS'
            allocate(equilibrium_out%code%output_flag(1))
            equilibrium_out%code%output_flag(1) = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure

        else
            ! ERROR IF THE CODE DOES NOT COMPLETE TO THE END
            status_code = -1
            allocate(character(50):: status_message)
            status_message = 'Error in physics_ii: input IDS not valid'
        endif

        ! FINAL DISPLAY
        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='
        write(*,*) ' '

        end subroutine code_lifecycle
end module mod_code_lifecycle
