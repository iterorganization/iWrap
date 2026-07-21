program standalone

    use iwrap_tools
    use muscle3_tools
    use mod_wrapped_code

    implicit none

    !---------------------------------------------------------
    character(len=*), parameter :: ACTOR_NAME = "m3_actor"
    type(LIBMUSCLE_Instance) :: instance
    integer :: process_rank


    !----  Status info  ----
    integer :: status_code = 0
	character(len=:), pointer :: status_message

    !----  Code parameters  ----
    character(len=:), allocatable :: xml_string
    type(ids_parameters_input) :: imas_code_params

    !----  IN/OUT IDSes  ----
    ! core_profiles_in
    type (ids_core_profiles) :: core_profiles_in
    ! distribution_sources_out
    type (ids_distribution_sources) :: distribution_sources_out

     ! > > > - - - INITIALISATION - - - < < <
    CALL init_muscle(instance)

    ! > > > - - - WRAPPED CODE CALL - INIT SBRT - - - < < <
    CALL  wrapped_code_init( status_code, status_message)
    CALL check_status(instance, status_code, status_message, ACTOR_NAME)

    ! > > >  MUSCLE3 LOOP. < < <
    do while (LIBMUSCLE_Instance_reuse_instance(instance))

        ! > > >  RECEIVING INPUT IDSes  < < <
        CALL recveive_input_idses(core_profiles_in, instance )

        ! > > > - - - WRAPPED CODE CALL - MAIN SBRT - - - < < <
        CALL  wrapped_code_main( core_profiles_in, distribution_sources_out, status_code, status_message)

        !-----------Check status info ---------------------
        CALL check_status(instance, status_code, status_message, ACTOR_NAME)

        ! > > >  SENDING OUTPUT IDSes  < < <
        CALL send_output_idses(distribution_sources_out, instance)

        ! > > >  CLEAN UP  < < <
         CALL ids_deallocate(core_profiles_in)
         CALL ids_deallocate(distribution_sources_out)

    end do ! < < < MUSCLE3 LOOP < < <

    ! > > > - - - - - - - - - - - - - WRAPPED CODE CALL - FINALISE SBRT - - - - - - - - - - - - - - - - - - < < <
    CALL  wrapped_code_finalise(status_code, status_message)
    !-----------Check status info ---------------------
    CALL check_status(instance, status_code, status_message, ACTOR_NAME)

end program