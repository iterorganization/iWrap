module mod_code1


character(len=:), pointer :: greeting

contains


    !===================================================================
    !                            INIT
    !===================================================================
    subroutine code1_setup (status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        write(*,*)
        write(*,*)
        write(*,*) " --- Code Fortran: INIT ---"

        ! Code specific initialization actions
        allocate(character(50) :: greeting)
        greeting = " Fortran: Hello!"

        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

    end subroutine code1_setup


    !===================================================================
    !                            MAIN
    !===================================================================
    subroutine code1_step(ids_in, ids_out, status_code, status_message)

        use ids_schemas, only: ids_core_profiles, ids_distribution_sources
        use ids_routines

        implicit none

        type(ids_core_profiles):: ids_in
        type(ids_distribution_sources):: ids_out
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        logical :: should_deallocate = .FALSE.

        write(*,*)
        write(*,*) "  Code Fortran: MAIN "

         ! Computations specific to the code
        ids_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS ! Mandatory field

        if (.not. associated(ids_out%ids_properties%comment)) allocate(ids_out%ids_properties%comment(1))
        ids_out%ids_properties%comment(1) = trim(ids_in%ids_properties%comment(1)) // trim(greeting) // " | "

        ! Setting status flag and message
        status_code = 0
        allocate(character(5):: status_message)
        status_message = 'OK'

    end subroutine code1_step


    !===================================================================
    !                            FINALIZE
    !===================================================================
    subroutine code1_cleanup(status_code, status_message)
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        write(*,*)
        write(*,*)
        write(*,*) " === Code Fortran: FINALIZE ==="

        ! Code specific finalization actions
        deallocate(greeting)
        greeting => null()

        ! Setting status flag and messageE
        status_code = 0
        allocate(character(5):: status_message)
        status_message = 'OK'

    end subroutine code1_cleanup


end module mod_code1