program helloworld
    use ids_routines
    use ymmsl
    use libmuscle
    implicit none





    type (ids_core_profiles) :: core_profiles_out
    type (ids_distribution_sources) :: distribution_sources_in

    real (selected_real_kind(15)) :: t_cur, t_next, t_max, dt
    character(len=1), dimension(:), allocatable :: serialized_ids
    real :: time

    type(LIBMUSCLE_PortsDescription) :: ports
    type(LIBMUSCLE_Instance) :: instance

    type(LIBMUSCLE_Message) :: rmsg
    type(LIBMUSCLE_DataConstRef) :: ritem

    type(LIBMUSCLE_Message) :: smsg
    type(LIBMUSCLE_Data) :: sitem

   character(:), allocatable :: test_dir_path

    write(*,*) "Starting Fortran M3 macro"

   test_dir_path = get_test_dir()

    open(10, file=test_dir_path//"/test.out", status="NEW")
    
    ports = LIBMUSCLE_PortsDescription_create()
    call LIBMUSCLE_PortsDescription_add(ports, YMMSL_OPERATOR_O_I, 'core_profiles_out')
    call LIBMUSCLE_PortsDescription_add(ports, YMMSL_OPERATOR_S, 'distribution_sources_in')
    instance = LIBMUSCLE_Instance_create(ports, &
            LIBMUSCLE_InstanceFlags(KEEPS_NO_STATE_FOR_NEXT_USE=.true.))
    call LIBMUSCLE_PortsDescription_free(ports)

    do while (LIBMUSCLE_Instance_reuse_instance(instance))
        ! F_INIT
        t_max = LIBMUSCLE_Instance_get_setting_as_real8(instance, 't_max')
        dt = LIBMUSCLE_Instance_get_setting_as_real8(instance, 'dt')

       time = 1

        t_cur = 0.0
        do while (t_cur + dt < t_max)
            ! O_I
            t_next = t_cur + dt


            ! SENDING DATA
            allocate(core_profiles_out%ids_properties%comment(1))
            core_profiles_out%ids_properties%comment(1) = 'Hello world!'
            core_profiles_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS
            allocate(core_profiles_out%code%name(1))
            core_profiles_out%code%name = 'HelloWorld Actor in Fortran'

            allocate(core_profiles_out%time(1))
            core_profiles_out%time(1) = t_next


            call ids_serialize(core_profiles_out, serialized_ids)
            call ids_deallocate(core_profiles_out)

            sitem = LIBMUSCLE_Data_create_byte_array(serialized_ids)
            smsg = LIBMUSCLE_Message_create(t_cur, sitem)

            if (t_next + dt <= t_max) then
                call LIBMUSCLE_Message_set_next_timestamp(smsg, t_next)
            end if
            call LIBMUSCLE_Instance_send(instance, 'core_profiles_out', smsg)
            call LIBMUSCLE_Message_free(smsg)
            call LIBMUSCLE_Data_free(sitem)
            deallocate(serialized_ids)


            ! > > > - - - RECEIVING DATA - - - < < <

            rmsg = LIBMUSCLE_Instance_receive(instance, 'distribution_sources_in')
            ritem = LIBMUSCLE_Message_get_data(rmsg);
    
            allocate(serialized_ids(LIBMUSCLE_DataConstRef_size(ritem)))
            call LIBMUSCLE_DataConstRef_as_byte_array(ritem, serialized_ids)
            call LIBMUSCLE_DataConstRef_free(ritem)

    
            ! deserialize and verify received IDS
            call ids_deserialize(serialized_ids, distribution_sources_in)
            deallocate(serialized_ids)

            if (.not. associated(distribution_sources_in%time)) then
                call LIBMUSCLE_Instance_error_shutdown(instance, "Received TIME is not associated")
                call exit(1)
            end if

            write(*,*) "TIME received ", distribution_sources_in%time
            write(10,*) distribution_sources_in%time

            time = distribution_sources_in%time(1)

            call ids_deallocate(distribution_sources_in)
                
            if (LIBMUSCLE_Message_has_next_timestamp(rmsg)) then
                t_max = LIBMUSCLE_Message_next_timestamp(rmsg)
            end if

            call LIBMUSCLE_Message_free(rmsg)

            ! a simulation would actually update something here, but we're just saying
            ! hi to connected actors and don't do anything else
            t_cur = t_cur + dt
        end do



    end do

    close(10)

contains

    FUNCTION get_test_dir() RESULT(test_dir_path)

        CHARACTER(:), allocatable :: test_dir_path
        CHARACTER(len=1000) :: tmp_str
        INTEGER :: var_size, status

        CALL get_environment_variable ("TEST_DIR", value=tmp_str, length=var_size, status=status)

        if (status .ne. 0) then
            write (*,*) 'Getting environment variable "TEST_DIR" failed: status = ', status
            stop
        end if

        ALLOCATE(character(len=var_size) :: test_dir_path)

        test_dir_path =tmp_str(1:var_size)

    END FUNCTION get_test_dir
end program helloworld
