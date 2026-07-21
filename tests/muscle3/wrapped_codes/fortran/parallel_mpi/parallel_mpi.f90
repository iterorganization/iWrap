module mod_parallel_mpi

    use mpi
    use ids_schemas

    integer :: code_state = 0

contains


            !
    !    INITIALISATION SUBROUTINE
    !
    subroutine init_code (status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: mpi_world_size, mpi_rank, error


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        call MPI_Comm_size ( MPI_COMM_WORLD, mpi_world_size, error )
        call MPI_Comm_rank ( MPI_COMM_WORLD, mpi_rank, error )

        write(*,*) '======================================='
        write(*,*) 'Parallel MPI:: INITIALISATION called'
        write(*,*) 'Process <', mpi_rank, '> out of: ', mpi_world_size
        write(*,*) '======================================='

    end subroutine init_code


    !
    !    FINALISATION SUBROUTINE
    !
    subroutine clean_up(status_code, status_message)
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: mpi_world_size, mpi_rank, error

        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        call MPI_Comm_size ( MPI_COMM_WORLD, mpi_world_size, error )
        call MPI_Comm_rank ( MPI_COMM_WORLD, mpi_rank, error )

        write(*,*) '======================================='
        write(*,*) 'Parallel MPI: FINALISATION called'
        write(*,*) 'Process <', mpi_rank, '> out of: ', mpi_world_size
        write(*,*) '======================================='

    end subroutine clean_up

    subroutine code_step(coreprofilesin, distsourceout, error_flag, error_message)

        implicit none

        integer,parameter :: DP=kind(1.0D0)

        type (ids_core_profiles) :: coreprofilesin
        type (ids_distribution_sources) :: distsourceout
        integer, intent(out) :: error_flag
        character(len=:), pointer, intent(out) :: error_message

        integer :: i
        integer :: mpi_world_size, mpi_rank, error

        write(0,*) 'Entering subroutine parallel MPI: code_step'

          ! INITIALISATION OF ERROR FLAG
          error_flag = 0
          allocate(character(50):: error_message)
          error_message = 'Status info of parallel_mpi'

        call MPI_Comm_size ( MPI_COMM_WORLD, mpi_world_size, error )
        call MPI_Comm_rank ( MPI_COMM_WORLD, mpi_rank, error )

        ! The output IDS  must be allocated with its number of time slices (1 for a single time slice physics module)
        ! Here we allocate the output IDS  to the same size as the input IDS (but this is not a general rule)

        write(*,*) '======================================='
        write(*,*) 'Parallel MPI: STEP called'
        write(*,*) 'Process <', mpi_rank, '> out of: ', mpi_world_size
        write(*,*) '======================================='

        write(*,*) 'Starting from: ', code_state

        do i = 1, 20
            ! COMPUTATIONS
            code_state = code_state + 1
        end do

        write(*,*) 'Counting to: ', code_state

        if (associated(coreprofilesin%time)) then

            allocate(distsourceout%time(size(coreprofilesin%time)))
            write(0,*) 'Received size of input time from coreprofilesin : ', SIZE(coreprofilesin%time)

            ! Fill in the output IDS (Physical data)
            do i=1,size(coreprofilesin%time)
               ! Time : copy from input IDS
               distsourceout%time(i) = 1000000 * mpi_rank + 100 * code_state + coreprofilesin%time(i) + 1
               ! THE TIME FIELD MUST BE FILLED (MANDATORY) in case of multiple time slice mode for the IDS;

            enddo
        else
            allocate(distsourceout%time(1))
            distsourceout%time(1) = 1000000 * mpi_rank + 100 * code_state
        end if


        distsourceout%ids_properties%homogeneous_time = 1

        allocate(distsourceout%code%name(1))   ! For a string of 132 characters max.
        distsourceout%code%name(1)   = 'parallel_mpi'
        allocate(distsourceout%code%version(1))   ! For a string of 132 characters max.
        distsourceout%code%version(1)   = '1.0'
        allocate(distsourceout%code%output_flag(1))
        distsourceout%code%output_flag(1) = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure

        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='

        return
    end subroutine


    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   GET TIMESTAMP SUBROUTINE
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine get_timestamp(timestamp_out, error_flag, error_message)

    integer,parameter :: DP=kind(1.0D0)
    real(kind=DP), intent(out) :: timestamp_out
    !----  Status info  ----
    integer, intent(out) :: error_flag
    character(len=:), pointer, intent(out) :: error_message
    integer :: mpi_world_size, mpi_rank, error

     error_flag = 0
    timestamp_out = code_state

    end subroutine get_timestamp

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   GET STATE SUBROUTINE
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine get_code_state (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(out) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: mpi_world_size, mpi_rank, error


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        allocate(character(50):: state_str)
        write(state_str,*) code_state

        call MPI_Comm_size ( MPI_COMM_WORLD, mpi_world_size, error )
        call MPI_Comm_rank ( MPI_COMM_WORLD, mpi_rank, error )

        write(*,*) '======================================='
        write(*,*) 'Parallel MPI:: GET CODE STATE called'
        write(*,*) 'Process <', mpi_rank, '> out of: ', mpi_world_size
        write(*,*) 'STATE is :', state_str
        write(*,*) '======================================='

    end subroutine get_code_state


    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   SET STATE SUBROUTINE
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine restore_code_state (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(in) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: mpi_world_size, mpi_rank, error

        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        call MPI_Comm_size ( MPI_COMM_WORLD, mpi_world_size, error )
        call MPI_Comm_rank ( MPI_COMM_WORLD, mpi_rank, error )

        read(state_str , *) code_state
        write(*,*) '======================================='
        write(*,*) 'Parallel MPI: RESTORE STATE called'
        write(*,*) 'STATE TO BE RESTORED :', code_state
        write(*,*) 'Process <', mpi_rank, '> out of: ', mpi_world_size
        write(*,*) '======================================='

    end subroutine restore_code_state
end module mod_parallel_mpi
