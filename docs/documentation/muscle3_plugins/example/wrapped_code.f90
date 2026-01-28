module mod_wrapped_code
contains

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine wrapped_code_init(status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        write(*,*) '======================================='
        write(*,*) 'Wrapped code: INITIALISATION called'
        write(*,*) '======================================='

    end subroutine wrapped_code_init


    !
    !    MAIN SUBROUTINE
    !
    subroutine wrapped_code_main(coreprofilesin, distsourceout, error_flag, error_message)
        use ids_schemas
        implicit none

        type (ids_core_profiles) :: coreprofilesin
        type (ids_distribution_sources) :: distsourceout
        integer, intent(out) :: error_flag
        character(len=:), pointer, intent(out) :: error_message

        write(*,*) '======================================='
        write(*,*) 'Wrapped code: MAIN called'
        write(*,*) '======================================='

        !COMPUTATIONS
        allocate(distsourceout%time(1))
        distsourceout%time(1) = coreprofilesin%time(1) + 1
        distsourceout%ids_properties%homogeneous_time = 1

        return
    end subroutine wrapped_code_main

    !
    !    FINALISATION SUBROUTINE
    !
    subroutine wrapped_code_finalise(status_code, status_message)
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        write(*,*) '======================================='
        write(*,*) 'Wrapped code: FINALISATION called'
        write(*,*) '======================================='

    end subroutine wrapped_code_finalise

end module mod_wrapped_code


