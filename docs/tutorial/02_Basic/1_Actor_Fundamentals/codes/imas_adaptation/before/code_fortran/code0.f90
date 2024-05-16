program code0

    implicit none
    character(:), allocatable :: greet_msg

    greet_msg = greet()

    write(*,*) "Message: ", greet_msg


contains
    function greet()
      character(:), allocatable :: greet

      allocate(character(5) :: greet)
      greet = 'Hello from pure Fortran'

    end function greet
  
end program code0
