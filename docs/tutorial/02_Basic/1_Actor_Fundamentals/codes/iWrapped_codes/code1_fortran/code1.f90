module mod_code1


character(len=:), pointer :: greeting

contains
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


        write(*,*) " Code Fortran: MAIN"

        ! Code specific initialization actions
        allocate(character(50) :: greeting)
        greeting = " Fortran: Hello!"

        ! Computations specific to the code
        ids_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS ! Mandatory field

        if (.not. associated(ids_out%ids_properties%comment)) allocate(ids_out%ids_properties%comment(1))
        ids_out%ids_properties%comment(1) = trim(ids_in%ids_properties%comment(1)) //trim(greeting) // " | "


        ! Code specific finalization actions
        deallocate(greeting)

        ! Setting status flag and message
        status_code = 0
        allocate(character(5):: status_message)
        status_message = 'OK'

    end subroutine code1_step


end module mod_code1