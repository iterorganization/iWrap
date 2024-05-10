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
        type(C_PTR) :: status_message
        integer :: str_len, istat

        !-----------Writing output data to file ---------------------
        open(10,file=file_name ,form='formatted',access='sequential',status='unknown', iostat=istat)
        if (istat /=0) then
            write(*,*) 'Error opening file: ', file_name
            return
        end if
        call writefile(status_code)

        if ( C_ASSOCIATED(status_message)) then
            str_len = c_str_length(status_message) + 1
            call writefile(str_len)
            call writefile(convert_cptr2string(status_message))
        else
            call writefile(0)
            call writefile("")
        end if
        close(10)

    END SUBROUTINE write_output

    SUBROUTINE open_db_entries(db_entry_desc_array)

        use ids_routines

        type(ids_description_t), dimension(:), intent(INOUT) :: db_entry_desc_array
        integer :: i, j

        db_entry_desc_array(:)%idx = -1
   do i=1, SIZE(db_entry_desc_array)
      j=1
      do while (j.lt.i)
         if ( db_entry_desc_array(j)%shot .eq. db_entry_desc_array(i)%shot .and.       &
              db_entry_desc_array(j)%run .eq. db_entry_desc_array(i)%run .and.         &
              convert_array2string(db_entry_desc_array(j)%user) .eq. convert_array2string(db_entry_desc_array(i)%user) .and.       &
              convert_array2string(db_entry_desc_array(j)%machine) .eq. convert_array2string(db_entry_desc_array(i)%machine).and. &
              convert_array2string(db_entry_desc_array(j)%version) .eq. convert_array2string(db_entry_desc_array(i)%version) ) then
            EXIT
         else
            j=j+1
         end if
      end do
      if (j.eq.i) then
         call imas_open_env("", db_entry_desc_array(i)%shot, &
                                db_entry_desc_array(i)%run, &
                                db_entry_desc_array(i)%idx, &
                        convert_array2string(db_entry_desc_array(i)%user), &
                        convert_array2string(db_entry_desc_array(i)%machine), &
                        convert_array2string(db_entry_desc_array(i)%version) )
      else
         db_entry_desc_array(i)%idx = db_entry_desc_array(j)%idx
      end if
   end do
    END SUBROUTINE open_db_entries



    SUBROUTINE close_db_entries(db_entry_desc_array)

        use ids_routines


        type(ids_description_t), dimension(:), intent(INOUT) :: db_entry_desc_array
        integer :: i, j
          do i=1, SIZE(db_entry_desc_array)
      j=1
      do while (j.lt.i)
         if (db_entry_desc_array(j)%idx.eq.db_entry_desc_array(i)%idx) then
            EXIT
         else
            j=j+1
         end if
      end do
      if (j.eq.i) then
         call imas_close(db_entry_desc_array(i)%idx)
      end if
   end do
    END SUBROUTINE close_db_entries

end module iwrap_tools


