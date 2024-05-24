module rwtool
       use iwrap_defs

   interface readfile
       module procedure &
          readids,&
          read_string,&
          readint
   end interface

   interface writefile
       module procedure &
           writeids,&
           writeint,&
           writechar
   end interface writefile

   contains

   subroutine readids(ids_description)

    implicit none
    type(ids_description_t), intent(inout) :: ids_description

        read(10,*) ! skip line " ----- IDS ----- "
        call read_chars( ids_description%ids_name)
        read(10,*) ids_description%shot
        read(10,*) ids_description%run
        read(10,*) ids_description%occurrence
        read(10,*) ids_description%backend_id
        read(10,*) ids_description%idx
        call read_chars( ids_description%machine)
        call read_chars( ids_description%user)
        call read_chars( ids_description%version)
   end subroutine


   subroutine readint(var)
     integer, intent(inout) :: var
     read(10,*) var
   end subroutine

   !---------------------------------------------------
   subroutine read_chars(var)
        implicit none
        character(kind=c_char),dimension(:),intent(inout) :: var
        integer                 :: i
        character(AL_STRING_SIZE)  :: line
        
        var(:) = char(0)
        read(10,"(a)")  line
        
        ! -- convert string -> array
        do i = 1, AL_STRING_SIZE
            var(i) = line(i : i)
        enddo

   end subroutine

    FUNCTION read_file(filename, str) RESULT(status)
        implicit none

        character(len=*),intent(in) :: filename
        character(len=:), pointer, intent(out) :: str

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
   !---------------------------------------------------
   subroutine read_string(var, isize)
    implicit none
     !character,dimension(:),intent(inout) :: var
     character(len=:), allocatable :: var
     integer :: isize
     character(1000)                 :: line
     integer :: k, i, line_length, read_count, io_stat
     var(:) = ''
     read_count = 1
     io_stat = 0

     DO WHILE (read_count < isize .and. io_stat > -1)
       line = char(10)
       read(10,"(a)", iostat=io_stat)  line

       line_length = len(line)
       do i = 1, line_length
           print *, i, " : ",  iachar(line(i:i)), " : ", line(i : i)
       enddo

       line_length = len_trim(line)
       if(read_count + line_length > isize) then
        line_length = isize - read_count
       endif

        print *, "Line length: ", line_length
       ! -- convert string -> array
       do i = 1, line_length
           var(read_count:read_count) = line(i : i)
           read_count= read_count + 1
       enddo

       ! -- add a new line at the end of line
       var(read_count:read_count) = char(10)
       print *, "New line at: " , read_count
       read_count = read_count + 1
    END DO


   end subroutine


   
   !========================================================================
   
   subroutine writeids(ids_description)

    implicit none
    type(ids_description_t), intent(in) :: ids_description
    write(10,*) ids_description%ids_name
    write(10,*) ids_description%shot
    write(10,*) ids_description%run
    write(10,*) ids_description%occurrence
    write(10,*) ids_description%backend_id
    write(10,*) ids_description%idx
    write(10,*) ids_description%machine
    write(10,*) ids_description%user
    write(10,*) ids_description%version
   end subroutine
   
   !---------------------------------------------------
   subroutine writeint(var)
     integer, intent(in) :: var
     write(10,*) var
   end subroutine

   !---------------------------------------------------
   subroutine writechar(string)
     character(*), intent(IN) :: string
     write(10, '(a)') string
   end subroutine
end module
