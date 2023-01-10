MODULE mod_timestamps

    real :: timestamp = 0

contains


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                                   GET TIMESTAMP SUBROUTINE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
subroutine get_timestamp(timestamp_out, error_flag, error_message)

    integer,parameter :: DP=kind(1.0D0)
    real(kind=DP), intent(out) :: timestamp_out
    !----  Status info  ----
    integer, intent(out) :: error_flag
    character(len=:), pointer, intent(out) :: error_message

    timestamp_out = timestamp

end subroutine get_timestamp

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                                   MAIN SUBROUTINE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
subroutine timestamps(coreprofilesin, distsourceout, error_flag, error_message)

use ids_schemas

implicit none

integer,parameter :: DP=kind(1.0D0)

type (ids_core_profiles) :: coreprofilesin
type (ids_distribution_sources) :: distsourceout
integer, intent(out) :: error_flag
character(len=:), pointer, intent(out) :: error_message

integer :: i

write(0,*) 'Entering subroutine eq2dist'

write(0,*) 'size of input IDS  = ',size(coreprofilesin%time)

  ! INITIALISATION OF ERROR FLAG
  error_flag = 0
  allocate(character(50):: error_message)
  error_message = 'Status info of coreprofiles2distsource'

! The output IDS  must be allocated with its number of time slices (1 for a single time slice physics MODULE)
! Here we allocate the output IDS  to the same size as the input IDS (but this is not a general rule)
allocate(distsourceout%time(size(coreprofilesin%time)))


write(0,*) 'Received size of input time from equilibrium : ', SIZE(coreprofilesin%time)

! Fill in the output IDS (Physical data)
do i=1,size(coreprofilesin%time)
   ! Time : copy from input IDS
   distsourceout%time(i) = coreprofilesin%time(i)  
   ! THE TIME FIELD MUST BE FILLED (MANDATORY) in case of multiple time slice mode for the IDS;
    
enddo

distsourceout%ids_properties%homogeneous_time = 1

timestamp = timestamp + 1

write(0,*) 'End Subroutine'

return
end subroutine

end MODULE mod_timestamps
