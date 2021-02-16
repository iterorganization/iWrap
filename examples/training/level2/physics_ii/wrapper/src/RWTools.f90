! Generated with FC2K version 4.13.0
module rwtool
   use idsmodule

   interface readfile
       module procedure &
          readids,&
          readint,&
          readtabint,&
          readlong,&
          readtablong,&
          readdouble,&
          readtabdouble,&
          readchar,&
          readtabchar
   end interface

   interface writefile
       module procedure &
           writeids,&
           writeint,&
           writetabint,&
           writelong,&
           writetablong,&
           writedouble,&
           writetabdouble,&
           writechar, &
           writetabchar
   end interface

   contains

   subroutine readids(Mids)
    use idsmodule
    implicit none
    type(idsstruct), intent(inout) :: Mids
    read(10,*) Mids%ids
    read(10,*) Mids%shot
    read(10,*) Mids%run
    read(10,*) Mids%occurrence
    read(10,*) Mids%idx
    read(10,*) Mids%machine
    read(10,*) Mids%user
    read(10,*) Mids%version
   end subroutine
   subroutine readint(var)
     use idsmodule
     integer, intent(inout) :: var
     read(10,*) var
   end subroutine
   
   !---------------------------------------------------
   subroutine readtabint(var)
      use idsmodule
      integer,dimension(:),intent(inout) :: var
      integer :: i
      do i=1,size(var)
        read(10,*) var(i)
      enddo
   end subroutine
   
   !---------------------------------------------------
   subroutine readlong(var)
     use idsmodule
     integer*8, intent(inout) :: var
     read(10,*) var
   end subroutine
   
   !---------------------------------------------------
   subroutine readtablong(var)
     use idsmodule
     integer*8,dimension(:),intent(inout) :: var
     integer :: i
      do i=1,size(var)
        read(10,*) var(i)
      enddo
   end subroutine
   
   !---------------------------------------------------
   subroutine readdouble(var)
     use idsmodule
     real*8, intent(inout) :: var
     read(10,*) var
   end subroutine
   
   !---------------------------------------------------
   subroutine readtabdouble(var)
     use idsmodule
     real*8,dimension(:),intent(inout) :: var
     do i=1,size(var)
        read(10,*) var(i)
     enddo
   end subroutine
   
   !---------------------------------------------------

   subroutine readtabchar(var, isize)
    implicit none
     character,dimension(:),intent(inout) :: var
     integer :: isize
     character(1000)                 :: line
     integer :: k, i, line_length, read_count
     var(:) = ''
     read_count = 1

     do
       line = ""
       read(10,"(a)")  line

       ! -- if line consist a string "FC2KENDOFSTRING" --> exit the loop
       k = index( line , "FC2KENDOFSTRING" )
       if( k /= 0 ) then
          exit
       endif

       line_length = len_trim(line)
       if(read_count + line_length > isize) then
        line_length = isize - read_count
       endif

       ! -- convert string -> array
       do i = 1, line_length
           var(read_count) = line(i : i)
           read_count= read_count + 1
       enddo

       ! -- add a new line at the end of line
       var(read_count) = char(10)
       read_count = read_count + 1
       write(*,*) var
    enddo

        !- - removing endln from very last line
     do i = isize, 1, -1
           if( var(i) /= ' ') then
               if(var(i) == char(10)) then
                  var(i) = ''
               endif
               exit
           endif
      enddo

   end subroutine

   
   !---------------------------------------------------
   subroutine readchar(var,isize)
     use idsmodule
     integer :: isize
     character(isize),intent(inout) :: var
     character(1000)                 :: ligne
     integer::k,j
     j=1
     do
       ligne = ""
       read(10,"(a)")  ligne
       k = index( ligne , "FC2KENDOFSTRING" )
      ! si k n'est pas 0 c'est que ligne contient "FC2KENDOFSTRING"
       if( k /= 0 ) then 
            exit
       else
            if (len_trim(ligne)==0) then
                  var(j:j)=char(10)
                  j=j+1
            else
                  !var(j:len_trim(ligne))=ligne(1:len_trim(ligne))
                  var = var(1:j-1) // trim(ligne)
                  j=j+len_trim(ligne)   
            endif
       endif
     enddo
   end subroutine
   
   !========================================================================
   
   subroutine writeids(Mids)
    use idsmodule
    implicit none
    type(idsstruct), intent(inout) :: Mids
    write(10,*) Mids%ids
    write(10,*) Mids%shot
    write(10,*) Mids%run
    write(10,*) Mids%occurrence
    write(10,*) Mids%idx
    write(10,*) Mids%machine
    write(10,*) Mids%user
    write(10,*) Mids%version
   end subroutine
   
   !---------------------------------------------------
   subroutine writeint(var)
     use idsmodule
     integer, intent(inout) :: var
     write(10,*) var
   end subroutine
   subroutine writetabint(var)
      use idsmodule
      integer,dimension(:),intent(inout) :: var
      integer :: i
      do i=1,size(var)
        write(10,*) var(i)
      enddo
   end subroutine
   
   !---------------------------------------------------
   subroutine writelong(var)
     use idsmodule
     integer*8, intent(inout) :: var
     write(10,*) var
   end subroutine
   
   !---------------------------------------------------
   subroutine writetablong(var)
     use idsmodule
     integer*8,dimension(:),intent(inout) :: var
     integer :: i
      do i=1,size(var)
        write(10,*) var(i)
      enddo
   end subroutine
   
   !---------------------------------------------------
   subroutine writedouble(var)
     use idsmodule
     real*8, intent(inout) :: var
     write(10,*) var
   end subroutine
   
   !---------------------------------------------------
   subroutine writetabdouble(var)
     use idsmodule
     real*8,dimension(:),intent(inout) :: var
     do i=1,size(var)
        write(10,*) var(i)
     enddo
   end subroutine
   
   !---------------------------------------------------
   subroutine writetabchar(var)
     character,dimension(:),intent(inout) :: var
     do i=1,size(var)
        write(10,*) var(i)
     enddo
   end subroutine
   
   !---------------------------------------------------
   subroutine writechar(var,isize)
      use idsmodule
     integer :: isize
     character(isize),intent(inout) :: var
     write(10,*) var
   end subroutine
end module
