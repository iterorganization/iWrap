module mod_coreprofiles2distsource
contains
subroutine coreprofiles2distsource(coreprofilesin, distsourceout, error_flag, error_message)

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

! The output IDS  must be allocated with its number of time slices (1 for a single time slice physics module)
! Here we allocate the output IDS  to the same size as the input IDS (but this is not a general rule)
allocate(distsourceout%time(size(coreprofilesin%time)))

! Fill in the output IDS (Physical data)
do i=1,size(coreprofilesin%time)
   ! Time : copy from input IDS
   write(0,*) 'Received input time from equilibrium : ', coreprofilesin%time(i)
   distsourceout%time(i) = coreprofilesin%time(i)  
   ! THE TIME FIELD MUST BE FILLED (MANDATORY) in case of multiple time slice mode for the IDS;
    
enddo

distsourceout%ids_properties%homogeneous_time = 1

allocate(distsourceout%code%name(1))   ! For a string of 132 characters max.
distsourceout%code%name(1)   = 'equ2dist'
allocate(distsourceout%code%version(1))   ! For a string of 132 characters max.
distsourceout%code%version(1)   = '1.0'
allocate(distsourceout%code%parameters(1))   ! For a string of 132 characters max.
distsourceout%code%parameters(1) = 'my_code_specific_parameters'
distsourceout%code%output_flag = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure

write(0,*) 'End Subroutine'

return
end subroutine
end module mod_coreprofiles2distsource
