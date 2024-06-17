module iwrap_tools
    use iso_c_binding
    use iwrap_defs
    use iwrap_converters

    implicit none

    contains



    FUNCTION create_ids_full_name(ids_description)  RESULT (ids_full_name)

        implicit none

        type(ids_description_t), intent(IN) :: ids_description
        character(132):: ids_full_name

        if (ids_description%occurrence/=0) then
                write(ids_full_name,"(i0)") ids_description%occurrence
                ids_full_name=trim(convert_array2string(ids_description%ids_name))//"/"//trim(ids_full_name)
        else
                ids_full_name=trim(convert_array2string(ids_description%ids_name))
        endif
    END FUNCTION create_ids_full_name


    SUBROUTINE handle_status_info(status_code, status_message, actor_name, sbrt_name)
            !----  Status info  ----
        integer :: status_code
        type(C_PTR) :: status_message
        character(*) :: actor_name
        character(*) :: sbrt_name
        character(kind = C_CHAR), dimension(:), pointer :: status_info_array
        integer  :: status_info_size

        if((.NOT. c_associated(status_message)) .OR. (c_str_length(status_message) < 1)) then
            allocate(status_info_array(23))
            status_info_size = 23
            status_info_array = transfer("<No status information>", status_info_array)
        else
            status_info_size = c_str_length(status_message)
            call c_f_pointer(status_message, status_info_array, [status_info_size])
        endif
        print *, "---Diagnostic information returned from *** ", actor_name, " / ", sbrt_name, " ***:---"
        print *, "-------Output flag    : ", status_code
        print *, "-------Status info: ", status_info_array
        print *, "---------------------------------------------------------"
        ! deallocate(status_info_array)
    END SUBROUTINE handle_status_info

    FUNCTION read_input(file_name, db_entry_desc_array) RESULT(status)
        use rwtool
        character(*), intent(IN) :: file_name
        type(ids_description_t), dimension(:), intent(INOUT) :: db_entry_desc_array
        integer :: i, status
        integer :: ids_array_size, array_read_size

        status = 0
        ids_array_size = SIZE(db_entry_desc_array)

        open(10,file=file_name, form='formatted',access='sequential',status='old', iostat=status)
        if (status /=0) then
            write(*,*) 'Error opening file: ', file_name
            return
        end if

        read(10,*) ! skip line " === Arguments ===="
        read(10,*) ! skip line " Length:"
        read(10,*) array_read_size

        if ( ids_array_size /= array_read_size) then
            print *, "ERROR: Expected and read number of arguments differs (" ,  ids_array_size, " vs ", array_read_size, " )"
            status = -1
            return
        end if

        do i = 1, ids_array_size
             call readids(db_entry_desc_array(i))
        end do

        close(10)

    END FUNCTION read_input

    FUNCTION read_code_parameters(xml_parameters) RESULT(status)
        use rwtool
        character(len=:), pointer, intent(OUT) :: xml_parameters
        integer :: status

        status = read_file("code_parameters.xml", xml_parameters)
        if (status /= 0) return

    END FUNCTION read_code_parameters

    SUBROUTINE write_output(file_name, status_code, status_message)
        use rwtool
        character(*), intent(IN) :: file_name
        integer, intent(IN) :: status_code
        character(*), intent(IN) :: status_message
        integer :: str_len, istat

        !-----------Writing output data to file ---------------------
        open(10,file=file_name ,form='formatted',access='sequential',status='unknown', iostat=istat)
        if (istat /=0) then
            write(*,*) 'Error opening file: ', file_name
            return
        end if
        call writefile(status_code)

        str_len = LEN(status_message)
        if ( str_len > 0 ) then
            call writefile(str_len + 1)
            call writefile(status_message)
        else
            call writefile(0)
            call writefile("")
        end if
        close(10)

    END SUBROUTINE write_output


{% if build_info.al_version.startswith('4.')   %}


    SUBROUTINE open_db(db_entry_desc, idx, status)
        use ual_low_level_wrap

        type(ids_description_t), intent(IN) :: db_entry_desc
        INTEGER, INTENT(OUT)  :: idx
        INTEGER, INTENT(OUT)  :: status

        if (db_entry_desc%backend_id == MEMORY_BACKEND) then
            idx = db_entry_desc%idx
            status = 0
            return
        end if


        CALL ual_begin_pulse_action(db_entry_desc%backend_id, &
                                    db_entry_desc%pulse, &
                                    db_entry_desc%run, &
                                    convert_array2string(db_entry_desc%user), &
                                    convert_array2string(db_entry_desc%db_name), &
                                    convert_array2string(db_entry_desc%version), &
                                    idx, &
                                    status)
        if (status .eq. 0)   CALL ual_open_pulse(idx, OPEN_PULSE, '', status)

       if (status /= 0) then
            write (*,*) "ERROR: Cannot open DB entry!", &
                        " BE: ", db_entry_desc%backend_id, &
                        " USER: ",   convert_array2string(db_entry_desc%user), &
                        " DB: ", convert_array2string(db_entry_desc%db_name), &
                        " PULSE/RUN: ",  db_entry_desc%pulse,  "/", db_entry_desc%run
                        return

       end if

    END SUBROUTINE open_db
{% else %}


    SUBROUTINE open_db(db_entry_desc, idx, status)
        use ids_routines

        type(ids_description_t), intent(IN) :: db_entry_desc
        INTEGER, INTENT(OUT)  :: idx
        INTEGER, INTENT(OUT)  :: status
        character (STRMAXLEN) :: uri


        if (db_entry_desc%backend_id == MEMORY_BACKEND) then
            idx = db_entry_desc%idx
            status = 0
            return
        end if

        call al_build_uri_from_legacy_parameters(db_entry_desc%backend_id, &
                                                 db_entry_desc%pulse, &
                                                 db_entry_desc%run, &
                                                 convert_array2string(db_entry_desc%user), &
                                                 convert_array2string(db_entry_desc%db_name), &
                                                 convert_array2string(db_entry_desc%version), &
                                                "", uri, status)

        if (status .eq. 0)  call imas_open( uri, OPEN_PULSE, idx, status)

        if (status /= 0) then
            write (*,*) "ERROR: Cannot open DB entry!", &
                        " BE: ", db_entry_desc_array(i)%backend_id, &
                        " USER: ",   convert_array2string(db_entry_desc_array(i)%user), &
                        " DB: ", convert_array2string(db_entry_desc_array(i)%db_name), &
                        " PULSE/RUN: ",  db_entry_desc_array(i)%pulse,  "/", db_entry_desc_array(i)%run
                        exit

        end if

    END SUBROUTINE open_db
{% endif %}

    SUBROUTINE close_db(db_entry_desc, idx)
        use ids_routines

        type(ids_description_t), intent(IN) :: db_entry_desc
        INTEGER, INTENT(OUT)  :: idx

        if (db_entry_desc%backend_id == MEMORY_BACKEND) then
            return
        end if

        call imas_close(idx)
    END SUBROUTINE close_db

end module iwrap_tools


