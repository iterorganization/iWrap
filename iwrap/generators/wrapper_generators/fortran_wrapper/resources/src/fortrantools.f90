module iwrap_tools
    use iwrap_defs
    use iso_c_binding

    implicit none

 interface convert
   module procedure &
       convert_codeparams,&
       convert_array2string,&
	   convert_string2array, &
       convert_cptr2string
 end interface convert

 
  interface convert2Cptr
   module procedure &
       convert_string2Cptr
 end interface

    interface
        function C_strlen(s) result(result) bind(C,name="strlen")
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
            length = C_strlen(c_string_ptr)
        end if
    end FUNCTION c_str_length

FUNCTION create_ids_full_name(ids_description)  RESULT (ids_full_name)

    implicit none

    type(ids_description_t), intent(IN) :: ids_description
    character(132):: ids_full_name

    if (ids_description%occurrence/=0) then
            write(ids_full_name,"(i0)") ids_description%occurrence
            ids_full_name=trim(ids_description%ids_name)//"/"//trim(ids_full_name)
    else
            ids_full_name=trim(ids_description%ids_name)
    endif
END FUNCTION create_ids_full_name

    SUBROUTINE handle_status_info(status_info, actor_name)
            !----  Status info  ----
        type(status_t) :: status_info
        character(*) :: actor_name
        character(kind = C_CHAR), dimension(:), pointer :: status_info_array
        integer  :: status_info_size

        if((.NOT. c_associated(status_info%message)) .OR. (c_str_length(status_info%message) < 1)) then
            allocate(status_info_array(23))
            status_info_size = 23
            status_info_array = transfer("<No status information>", status_info_array)
        else
            status_info_size = c_str_length(status_info%message)
            call c_f_pointer(status_info%message, status_info_array, [status_info_size])
        endif
        print *, "---Diagnostic information returned from *** ", actor_name, " ***:---"
        print *, "-------Output flag    : ", status_info%code
        print *, "-------Status info: ", status_info_array
        print *, "---------------------------------------------------------"
    END SUBROUTINE handle_status_info


    FUNCTION read_input(db_entry_desc_array, xml_string) RESULT(status)
        use rwtool
        type(ids_description_t), dimension(:), intent(INOUT) :: db_entry_desc_array
        character(len=:), allocatable, intent(OUT) :: xml_string
        integer :: i, status
        integer :: ids_array_size, array_read_size, xml_read_size

        status = 0
        ids_array_size = SIZE(db_entry_desc_array)

        open(10,file='input.txt',form='formatted',access='sequential',status='old', iostat=status)

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

        read(10,*) ! skip line " == Code Parameters =="
        read(10,*) ! skip line " Length:"

        read(10,*) xml_read_size

        if ( xml_read_size < 1)  then
            close(10)
            return
        end if

        read(10,*) ! skip line " --- Value: ---"


        allocate( character(len=xml_read_size) :: xml_string )
        call read_string(xml_string, xml_read_size)
        close(10)

    END FUNCTION read_input


    SUBROUTINE write_output(status_info)
        use rwtool
        type(status_t), intent(IN) :: status_info
        integer :: str_len, istat

        !-----------Writing output data to file ---------------------
        open(10,file='output.txt',form='formatted',access='sequential',status='unknown', iostat=istat)
        call writefile(status_info%code)

        if ( C_ASSOCIATED(status_info%message)) then
            str_len = c_str_length(status_info%message)
            call writefile(str_len)
            call writefile(convert_cptr2string(status_info%message))
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
              db_entry_desc_array(j)%user .eq. db_entry_desc_array(i)%user .and.       &
              db_entry_desc_array(j)%machine .eq. db_entry_desc_array(i)%machine .and. &
              db_entry_desc_array(j)%version .eq. db_entry_desc_array(i)%version ) then
            EXIT
         else
            j=j+1
         end if
      end do
      if (j.eq.i) then
         call imas_open_env("",db_entry_desc_array(i)%shot,db_entry_desc_array(i)%run,&
              db_entry_desc_array(i)%idx,db_entry_desc_array(i)%user,db_entry_desc_array(i)%machine,db_entry_desc_array(i)%version)
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

FUNCTION read_codeparams_schema(xsd_file)  RESULT (xsd_string)
    use rwtool
    implicit none

    character(len=*), intent(IN) :: xsd_file
    character(len=:), allocatable :: xsd_string

    if ( LEN(xsd_file) < 1 ) return

    xsd_string = read_file(xsd_file)

END FUNCTION read_codeparams_schema


FUNCTION convert_codeparams(code_params)  RESULT (xmllib_code_params)    
    use iso_c_binding, ONLY: C_PTR
    use ids_schemas, ONLY: ids_parameters_input
    implicit none


    type(code_parameters_t) :: code_params
    type(ids_parameters_input) :: xmllib_code_params

    type(C_PTR)      :: c_str_ptr  

    character(kind=C_CHAR), dimension(:), pointer :: f_char_arr 

    
    integer     :: iloopmax, string_size, sizexml,sizexsd
    
    !  xml parameters
    c_str_ptr = code_params%params
    string_size = c_str_length(c_str_ptr)
    iloopmax=string_size/132
    if (mod(string_size,132)/=0) then
    iloopmax = iloopmax + 1
    endif
    allocate(xmllib_code_params%parameters_value(iloopmax))
    
    call C_F_POINTER(c_str_ptr, f_char_arr, (/string_size/))
    xmllib_code_params%parameters_value = transfer(f_char_arr, xmllib_code_params%parameters_value)
    
    if(mod(string_size,132)/=0) then
    xmllib_code_params%parameters_value(iloopmax)(mod(string_size,132)+1:132) = ' '
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

    FUNCTION convert_string2Cptr(inString)  RESULT (outPtr)
        use iso_c_binding
        implicit none

        character(len=*), INTENT(IN) :: inString
        type(C_PTR) :: outPtr
        INTEGER :: i, strSize
        character, dimension(:), pointer :: outArray

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



end module iwrap_tools


