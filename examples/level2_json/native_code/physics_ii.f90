module mod_physics_ii

contains
subroutine physics_ii(equilibrium_in,equilibrium_out,codeparam,error_flag,error_message)

  ! ---------------------------------------
  ! PURPOSE: SIMPLE PSEUDO PHYSICS CODE 
  ! (IT READS AN EQUILIBRIUM IDS,
  ! MODIFIES ONE VARIABLE IN THE IDS, 
  ! AND WRITES THE NEW EQUILIBRIUM IDS)
  ! ---------------------------------------

  use ids_schemas, only: ids_equilibrium,ids_parameters_input,ids_is_valid
  use ids_routines, only: ids_copy
  use json_module
  use json_value_module

  implicit none

  type(ids_equilibrium):: equilibrium_in,equilibrium_out
  character(len=:), allocatable, intent(IN) :: codeparam
  integer, intent(out) :: error_flag
  character(len=:), pointer, intent(out) :: error_message

  integer :: ntimes
  real :: multiplication_factor

  type(json_file) :: json

  ! INITIALISATION OF ERROR FLAG
  error_flag = 0

  ! INITIAL DISPLAY
  write(*,*) ' '
  write(*,*) '======================================='
  write(*,*) 'START OF PHYSICS CODE'

  ! CHECK IF INPUT IDS IS VALID
  if (ids_is_valid(equilibrium_in%ids_properties%homogeneous_time).eqv..True. &
       .and.size(equilibrium_in%time)>0) then

    call json%deserialize(codeparam)

    call json%get('parameters.ntimes', ntimes)
    call json%get('parameters.multiplication_factor', multiplication_factor)

    write(*,*) '------------------------------------'
    write(*,*) 'Parameters read from input json file:'
    write(*,'(a25,i3)')   ' ntimes                = ',  &
         ntimes
    write(*,'(a25,f7.3)') ' multiplication_factor = ',&
         multiplication_factor
    write(*,*) '------------------------------------'

    ! COPY THE INPUT IDS IN THE OUTPUT IDS
    call ids_copy(equilibrium_in,equilibrium_out)

    ! INITIAL PLASMA MAJOR RADIUS
    write(*,'(a31,f7.3)') ' Initial plasma major radius = ', &
         equilibrium_in%time_slice(1)%boundary%geometric_axis%r

    ! MODIFY PLASMA MAJOR RADIUS
    equilibrium_out%time_slice(1)%boundary%geometric_axis%r = &
         equilibrium_out%time_slice(1)%boundary%geometric_axis%r &
         * ntimes * multiplication_factor

    ! MANDATORY FLAG (UNIFORM TIME HERE)
    equilibrium_out%ids_properties%homogeneous_time = 1

    ! FINAL PLASMA MAJOR RADIUS
    write(*,'(a31,f7.3)') ' Final plasma major radius   = ', &
         equilibrium_out%time_slice(1)%boundary%geometric_axis%r

  else

     ! ERROR IF THE CODE DOES NOT COMPLETE TO THE END
     error_flag = -1
     allocate(character(50):: error_message)
     error_message = 'Error in physics_ii: input IDS not valid'

  endif

  ! FINAL DISPLAY
  write(*,*) 'END OF PHYSICS CODE'
  write(*,*) '======================================='
  write(*,*) ' '

end subroutine physics_ii
end module mod_physics_ii
