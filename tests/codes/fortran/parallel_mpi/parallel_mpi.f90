module mod_parallel_mpi
contains
subroutine codeStep(core_profiles_in, distribution_sources_out, error_flag, error_message)
    use mpi
    use ids_schemas
    implicit none

    integer,parameter :: DP=kind(1.0D0)
    type (ids_core_profiles) :: core_profiles_in
    type (ids_distribution_sources) :: distribution_sources_out
    integer, intent(out) :: error_flag
    character(len=:), pointer, intent(out) :: error_message

    integer :: i
    integer :: mpi_world_size, mpi_rank, error

    write(0,*) 'Entering subroutine parallel_mpi::codeStep()'

    call MPI_Comm_size ( MPI_COMM_WORLD, mpi_world_size, error )
    call MPI_Comm_rank ( MPI_COMM_WORLD, mpi_rank, error )


    write (*,*) 'Info: process <', mpi_rank, '> out of: ', mpi_world_size


    write(0,*) 'size of input IDS  = ',size(core_profiles_in%time)

      ! INITIALISATION OF ERROR FLAG
      error_flag = 0
      allocate(character(50):: error_message)
      error_message = 'Status info of parallel_mpi::codeStep()'

    ! The output IDS  must be allocated with its number of time slices (1 for a single time slice physics module)
    ! Here we allocate the output IDS  to the same size as the input IDS (but this is not a general rule)
    allocate(distribution_sources_out%time(size(core_profiles_in%time)))


    write(0,*) 'Received size of input time from parallel_mpi::codeStep() : ', SIZE(core_profiles_in%time)

    ! Fill in the output IDS (Physical data)
    do i=1,size(core_profiles_in%time)
       ! Time : copy from input IDS
       distribution_sources_out%time(i) = 10000 * mpi_rank + core_profiles_in%time(i)
       ! THE TIME FIELD MUST BE FILLED (MANDATORY) in case of multiple time slice mode for the IDS;

    enddo

    distribution_sources_out%ids_properties%homogeneous_time = 1

    allocate(distribution_sources_out%code%name(1))   ! For a string of 132 characters max.
    distribution_sources_out%code%name(1)   = 'parallel_mpi'
    allocate(distribution_sources_out%code%version(1))   ! For a string of 132 characters max.
    distribution_sources_out%code%version(1)   = '1.0'
    allocate(distribution_sources_out%code%parameters(1))   ! For a string of 132 characters max.
    distribution_sources_out%code%parameters(1) = 'my_code_specific_parameters'
    allocate(distribution_sources_out%code%output_flag(size(core_profiles_in%time)))
    distribution_sources_out%code%output_flag(:) = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure

    write(0,*) 'End Subroutine'

    return
end subroutine codeStep
end module mod_parallel_mpi
