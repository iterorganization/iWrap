
{%- import './macros/flags.jinja2' as flags with context-%}

module iwrap_tools
   {% if flags.mpi_code %}
    use libmuscle_mpi
    {% else %}
    use libmuscle
    {% endif %}

    implicit none

contains

    SUBROUTINE check_status(instance, status_code, status_message, actor_name, actor_method)

        type(LIBMUSCLE_Instance), INTENT(IN) :: instance
        integer :: status_code
        character(*) :: actor_name
        character(*) :: actor_method
        character(:), pointer  :: status_message
        character(:), allocatable :: status_info

        if(associated(status_message))  then
            status_info = status_message
            deallocate(status_message)
        else
            status_info = "<No status information>"
        end if

        ! NO ERROR
        if(status_code == 0) return

        ! WARNING
        if(status_code > 0) then
            print *, "---WARNING returned from *** ", actor_name, "/", actor_method, " ***:---"
            print *, "-------Output flag    : ", status_code
            print *, "-------Status info: ", status_info
            print *, "---------------------------------------------------------"

        end if

        ! ERROR
        if(status_code < 0) then
            print *, "---WARNING returned from *** ", actor_name, "/", actor_method, " ***:---"
            print *, "-------Output flag    : ", status_code
            print *, "-------Status info: ", status_info
            print *, "---------------------------------------------------------"
            CALL LIBMUSCLE_Instance_error_shutdown(instance, "*"//actor_name//"/"//actor_method//"* error: "//status_info)
            CALL exit(status_code)
        end if

    END SUBROUTINE check_status


    FUNCTION get_exec_dir() RESULT(exec_path)
        CHARACTER(len=2055) :: exec_path
        integer :: idx

        CALL get_command_argument(0, exec_path)

        idx = index(trim(exec_path), '/', .True.)
        exec_path = exec_path(:idx)

    END FUNCTION get_exec_dir


    FUNCTION read_code_parameters_file(file_name, imas_code_params) RESULT(status)
        use ids_schemas, ONLY: ids_parameters_input
        character(len=:),allocatable, intent(in) :: file_name
        type(ids_parameters_input), intent(OUT)  :: imas_code_params
        character(len=:), allocatable :: code_parameters_str
        integer :: status
        integer     :: iloopmax, string_size, err_code

        status = read_file(file_name, code_parameters_str)
        if (status /= 0) return

        string_size = LEN_TRIM(code_parameters_str)

        iloopmax=string_size/132
        if (mod(string_size,132)/=0) then
            iloopmax = iloopmax + 1
        endif
        allocate(imas_code_params%parameters_value(iloopmax))

        imas_code_params%parameters_value = transfer(code_parameters_str(1:string_size), imas_code_params%parameters_value)

        if(mod(string_size,132)/=0) then
            imas_code_params%parameters_value(iloopmax)(mod(string_size,132)+1:132) = ' '
        endif

    END FUNCTION read_code_parameters_file

   !---------------------------------------------------
    FUNCTION read_file(filename, str) RESULT(status)
        implicit none

        character(len=*),intent(in) :: filename
        character(len=:),allocatable, intent(out) :: str

        !local variables:
        integer :: iunit,istat,filesize, status
        character(len=1) :: c

        status = 0

        open(newunit=iunit,file=filename,status='OLD',&
                form='UNFORMATTED',access='STREAM',iostat=istat)

        if (istat /=0) then
            write(*,*) 'Error opening file: ', filename
            status = -1
            return
        end if

        !how many characters are in the file:
        inquire(file=filename, size=filesize)
        if (filesize < 1) then
            write(*,*) 'Error getting file size: ', filename
            status = -1
            return
        end if

        !read the file all at once:
        allocate( character(len=filesize) :: str )
        read(iunit,pos=1,iostat=istat) str

        if (istat /=0 ) then
            write(*,*) 'Error reading file: ', filename
            status = -1
            return
        end if

        !make sure it was all read by trying to read more:
        read(iunit,pos=filesize+1,iostat=istat) c
        if (.not. IS_IOSTAT_END(istat)) &
            write(*,*) 'Error: file was not completely read.'

        close(iunit, iostat=istat)
   end function read_file

end module iwrap_tools


