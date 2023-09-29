module iwrap_tools
    use iwrap_defs
    use iso_c_binding

    implicit none

 interface convert
   module procedure &
       convert_array2string,&
	   convert_string2array, &
       convert_cptr2string
 end interface convert

 
  interface convert2Cptr
   module procedure &
       convert_string2Cptr
 end interface

    interface
        function iwrap_C_strlen(s) result(result) bind(C,name="strlen")
            use iso_c_binding

            integer(C_SIZE_T) :: result
            type(C_PTR), value, intent(in) :: s  !character(len=*), intent(in)
        end function
    end interface

contains


    FUNCTION c_str_length(c_string_ptr) result(length)
        use iso_c_binding

        integer(C_SIZE_T) :: length
        type(C_PTR), value, intent(in) :: c_string_ptr
        
        if (.not. C_ASSOCIATED(c_string_ptr)) then
            length = 0
        else
            length = iwrap_C_strlen(c_string_ptr)
        end if
    end FUNCTION c_str_length

FUNCTION create_ids_full_name(ids_description)  RESULT (ids_full_name)

    implicit none

    type(ids_description_t), intent(IN) :: ids_description
    character(132):: ids_full_name

    if (ids_description%occurrence/=0) then
            write(ids_full_name,"(i0)") ids_description%occurrence
            ids_full_name=trim(convert(ids_description%ids_name))//"/"//trim(ids_full_name)
    else
            ids_full_name=trim(convert(ids_description%ids_name))
    endif
END FUNCTION create_ids_full_name


    SUBROUTINE handle_status_info(status_code, status_message, actor_name)
            !----  Status info  ----
        integer :: status_code
        type(C_PTR) :: status_message
        character(*) :: actor_name
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
        print *, "---Diagnostic information returned from *** ", actor_name, " ***:---"
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
        character(len=:), allocatable, intent(OUT) :: xml_parameters
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
              convert(db_entry_desc_array(j)%user) .eq. convert(db_entry_desc_array(i)%user) .and.       &
              convert(db_entry_desc_array(j)%machine) .eq. convert(db_entry_desc_array(i)%machine).and. &
              convert(db_entry_desc_array(j)%version) .eq. convert(db_entry_desc_array(i)%version) ) then
            EXIT
         else
            j=j+1
         end if
      end do
      if (j.eq.i) then
         call imas_open_env("", db_entry_desc_array(i)%shot, &
                                db_entry_desc_array(i)%run, &
                                db_entry_desc_array(i)%idx, &
                        convert(db_entry_desc_array(i)%user), &
                        convert(db_entry_desc_array(i)%machine), &
                        convert(db_entry_desc_array(i)%version) )
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

FUNCTION convert_codeparams(code_params_cstr)  RESULT (al_code_params)
    use iso_c_binding, ONLY: C_PTR
    use ids_schemas, ONLY: ids_parameters_input
    implicit none

    type(ids_parameters_input) :: al_code_params

    character(kind=c_char), dimension(*), intent(IN) :: code_params_cstr

    character(kind=C_CHAR), dimension(:), pointer :: f_char_arr 

    
    integer     :: iloopmax, string_size
    

       string_size=0
    do
       if (code_params_cstr(string_size+1) == C_NULL_CHAR) exit
       string_size = string_size + 1
    end do


    iloopmax=string_size/132
    if (mod(string_size,132)/=0) then
    iloopmax = iloopmax + 1
    endif
    allocate(al_code_params%parameters_value(iloopmax))
    

    al_code_params%parameters_value = transfer(code_params_cstr(1:string_size), al_code_params%parameters_value)
    
    if(mod(string_size,132)/=0) then
    al_code_params%parameters_value(iloopmax)(mod(string_size,132)+1:132) = ' '
    endif

END FUNCTION convert_codeparams



FUNCTION convert_array2string(inArray)  RESULT (outString)    
implicit none
character, dimension(:), intent(IN) :: inArray
character(len=:), allocatable :: outString
integer :: i
integer(C_SIZE_T) :: strSize

strSize = SIZE(inArray)
allocate(character(strSize)::outString)


DO i = 1,SIZE(inArray)
   outString(i:i) = inArray(i)
END DO
END FUNCTION convert_array2string

! ------------------------

FUNCTION convert_string2array(inString)  RESULT (outArray) 
implicit none  
character(len=*), INTENT(IN) :: inString
character, dimension(:), pointer :: outArray
integer :: i
integer :: strSize

strSize = LEN(inString)
allocate(outArray(strSize))

DO i = 1,LEN(inString)
   outArray(i) = inString(i:i)
END DO
END FUNCTION convert_string2array

! ======================================================================================
! ======================================================================================

       FUNCTION convert_allocatable_string2c_ptr(inString)  RESULT (outPtr)
        use iso_c_binding
        implicit none

        character(len=:), allocatable, INTENT(IN) :: inString
        type(C_PTR) :: outPtr
        INTEGER :: i, strSize
        character, dimension(:), pointer :: outArray

        if(.not. allocated(inString)) then
             outPtr = C_NULL_PTR
             return
        end if

        strSize = LEN(inString)
        allocate(outArray(strSize + 1))

        DO i = 1,strSize
           outArray(i) = inString(i:i)
        END DO

        outArray(strSize + 1) =  C_NULL_CHAR
        outPtr = C_LOC(outArray)

    END FUNCTION convert_allocatable_string2c_ptr

    FUNCTION convert_string2Cptr(inString)  RESULT (outPtr)
        use iso_c_binding
        implicit none

        character(len=:), pointer, INTENT(IN) :: inString
        type(C_PTR) :: outPtr
        INTEGER :: i, strSize
        character, dimension(:), pointer :: outArray

        if(.not. associated(inString)) then
             outPtr = C_NULL_PTR
             return
        end if

        strSize = LEN(inString)
        allocate(outArray(strSize + 1))

        DO i = 1,strSize
           outArray(i) = inString(i:i)
        END DO

        outArray(strSize + 1) =  C_NULL_CHAR
        outPtr = C_LOC(outArray)

    END FUNCTION convert_string2Cptr

! ------------------------
FUNCTION convert_cptr2string(in_c_ptr) RESULT( out_string)
    use ISO_C_BINDING
    type(C_PTR), intent(in) :: in_c_ptr
    character(len=:), allocatable :: out_string
    character(len=1, kind=C_CHAR), dimension(:), pointer :: char_arr
    integer :: string_size
    if (.not. C_associated(in_c_ptr)) then
        out_string = ' '
        return
    endif

    string_size = c_str_length(in_c_ptr)
    call C_F_pointer(in_c_ptr, char_arr,  (/string_size/))

    allocate(character(string_size)::out_string)
    out_string = transfer(char_arr, out_string)

end FUNCTION

! ------------------------

FUNCTION convert_char_arr_to_al_str(in_char_arr) RESULT (al_str)
    ! converts long string (character(len>132)) to array of strings no longer than 132 characters
    use iso_c_binding, ONLY: C_NULL_CHAR
    implicit none

    character(kind=c_char), dimension(*), intent(IN) :: in_char_arr
    character(len=132), dimension(:), pointer :: al_str(:)

    integer :: iloopmax, string_size

    string_size=0
    do
       if (in_char_arr(string_size+1) == C_NULL_CHAR) exit
       string_size = string_size + 1
    end do

    iloopmax=string_size/132
    if (mod(string_size,132)/=0) then
        iloopmax = iloopmax + 1
    endif
    allocate(al_str(iloopmax))

    al_str = transfer(in_char_arr(1:string_size), al_str)

    if(mod(string_size,132)/=0) then
        al_str(iloopmax)(mod(string_size,132)+1:132) = ' '
    endif

END FUNCTION convert_char_arr_to_al_str

end module iwrap_tools


