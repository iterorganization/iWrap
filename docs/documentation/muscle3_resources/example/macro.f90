program macro
    use ids_routines
    use ymmsl
    use libmuscle
    implicit none

    type (ids_core_profiles) :: core_profiles_out
    type (ids_distribution_sources) :: distribution_sources_in

    real (selected_real_kind(15)) :: t_cur, t_next, t_max, dt
    character(len=1), dimension(:), allocatable :: serialized_ids

    type(LIBMUSCLE_PortsDescription) :: ports
    type(LIBMUSCLE_Instance) :: instance

    type(LIBMUSCLE_Message) :: rmsg
    type(LIBMUSCLE_DataConstRef) :: ritem

    type(LIBMUSCLE_Message) :: smsg
    type(LIBMUSCLE_Data) :: sitem


    ! > > > - - - INITIALISATION - - - < < <
    ports = LIBMUSCLE_PortsDescription_create()
    call LIBMUSCLE_PortsDescription_add(ports, YMMSL_OPERATOR_O_I, 'core_profiles_out')
    call LIBMUSCLE_PortsDescription_add(ports, YMMSL_OPERATOR_S, 'distribution_sources_in')
    instance = LIBMUSCLE_Instance_create(ports)
    call LIBMUSCLE_PortsDescription_free(ports)

    ! > > > - - - MAIN MUSCLE3 LOOP - - - < < <
    do while (LIBMUSCLE_Instance_reuse_instance(instance))
        ! F_INIT
        t_max = LIBMUSCLE_Instance_get_setting_as_real8(instance, 't_max')
        dt = LIBMUSCLE_Instance_get_setting_as_real8(instance, 'dt')

        t_cur = 0.0

        ! > > > - - - INTERNAL LOOP - - - < < <
        do while (t_cur + dt < t_max)
            ! O_I
            t_next = t_cur + dt


            ! > > > - - - MODEL COMPUTATIONS - - - < < <

            core_profiles_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS
            allocate(core_profiles_out%time(1))
            core_profiles_out%time(1) = 1.0

            !                   . . .
            ! > > > - - - - - - - - - - - - - - - < < <

            ! > > > - - - SENDING DATA - - - < < <
            call ids_serialize(core_profiles_out, serialized_ids)
            call ids_deallocate(core_profiles_out)

            sitem = LIBMUSCLE_Data_create_byte_array(serialized_ids)
            smsg = LIBMUSCLE_Message_create(t_cur, sitem)

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
            call LIBMUSCLE_Message_free(rmsg)

            call ids_deserialize(serialized_ids, distribution_sources_in)
            deallocate(serialized_ids)
            call ids_deallocate(distribution_sources_in)

            t_cur = t_cur + dt
        end do
    end do

end program macro