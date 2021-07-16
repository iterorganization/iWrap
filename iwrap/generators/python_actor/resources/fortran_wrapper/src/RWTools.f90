module rwtool
       use iwrap_defs

   interface readfile
       module procedure &
          readids,&
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
        read(10,*) ids_description%ids_name
        read(10,*) ids_description%shot
        read(10,*) ids_description%run
        read(10,*) ids_description%occurrence
        read(10,*) ids_description%idx
        read(10,*) ids_description%machine
        read(10,*) ids_description%user
        read(10,*) ids_description%version
   end subroutine


   subroutine readint(var)
     integer, intent(inout) :: var
     read(10,*) var
   end subroutine

FUNCTION read_file(filename) RESULT (str)

    implicit none

    character(len=*),intent(in) :: filename
    character(len=:),allocatable :: str

    !local variables:
    integer :: iunit,istat,filesize
    character(len=1) :: c

    open(newunit=iunit,file=filename,status='OLD',&
            form='UNFORMATTED',access='STREAM',iostat=istat)

    if (istat==0) then

        !how many characters are in the file:
        inquire(file=filename, size=filesize)
        if (filesize>0) then

            !read the file all at once:
            allocate( character(len=filesize) :: str )
            read(iunit,pos=1,iostat=istat) str

            if (istat==0) then
                !make sure it was all read by trying to read more:
                read(iunit,pos=filesize+1,iostat=istat) c
                if (.not. IS_IOSTAT_END(istat)) &
                    write(*,*) 'Error: file was not completely read.'
            else
                write(*,*) 'Error reading file.'
            end if

            close(iunit, iostat=istat)

        else
            write(*,*) 'Error getting file size.'
        end if
    else
        write(*,*) 'Error opening file.'
    end if

    end function read_file
   !---------------------------------------------------
   subroutine readtabint(var)

      integer,dimension(:),intent(inout) :: var
      integer :: i
      do i=1,size(var)
        read(10,*) var(i)
      enddo
   end subroutine


   
   !========================================================================
   
   subroutine writeids(ids_description)

    implicit none
    type(ids_description_t), intent(in) :: ids_description
    write(10,*) ids_description%ids_name
    write(10,*) ids_description%shot
    write(10,*) ids_description%run
    write(10,*) ids_description%occurrence
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
