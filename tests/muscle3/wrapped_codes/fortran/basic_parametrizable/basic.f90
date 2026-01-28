module mod_basic

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


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: INITIALISATION called'
        write(*,*) '======================================='

    end subroutine init_code


    !
    !    FINALISATION SUBROUTINE
    !
    subroutine clean_up(status_code, status_message)
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: FINALISATION called'
        write(*,*) '======================================='

    end subroutine clean_up

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_step(core_profiles_in, distribution_sources_out, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_is_valid

        implicit none

        type(ids_core_profiles):: core_profiles_in
        type(ids_distribution_sources):: distribution_sources_out
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: i

        ! INITIAL DISPLAY
        write(*,*) '======================================='
        write(*,*) 'START OF PHYSICS CODE'

        ! INITIALISATION OF ERROR FLAG
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'code_restart: OK'

        write(*,*) 'Starting from: ', code_state

        do i = 1, 20
            ! COMPUTATIONS
            code_state = code_state + 1
        end do

        write(*,*) 'Counting to: ', code_state

        distribution_sources_out%ids_properties%homogeneous_time = 1
        if (associated(core_profiles_in%time)) then

            allocate(distribution_sources_out%time(size(core_profiles_in%time)))
            write(0,*) 'Received size of input time from coreprofilesin : ', SIZE(core_profiles_in%time)

            ! Fill in the output IDS (Physical data)
            do i=1,size(core_profiles_in%time)
               ! Time : copy from input IDS
               distribution_sources_out%time(i) = 1000 * code_state + core_profiles_in%time(i)
               ! THE TIME FIELD MUST BE FILLED (MANDATORY) in case of multiple time slice mode for the IDS;

            enddo
        else
            allocate(distribution_sources_out%time(1))
            distribution_sources_out%time(1) = 1000 * code_state
        end if

        ! FINAL DISPLAY
        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='
        write(*,*) ' '

        end subroutine code_step

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   GET TIMESTAMP SUBROUTINE
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine get_timestamp(timestamp_out, error_flag, error_message)

    integer,parameter :: DP=kind(1.0D0)
    real(kind=DP), intent(out) :: timestamp_out
    !----  Status info  ----
    integer, intent(out) :: error_flag
    character(len=:), pointer, intent(out) :: error_message

     error_flag = 0
    timestamp_out = code_state

    end subroutine get_timestamp

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine get_code_state (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(out) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        allocate(character(50):: state_str)
        write(state_str,*) code_state



        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: GET CODE STATE called'
        write(*,*) 'STATE is :', state_str
        write(*,*) '======================================='

    end subroutine get_code_state

    subroutine restore_code_state (state_str, status_code, status_message)
    !
    !    INITIALISATION SUBROUTINE
    !
        implicit none
        character(len=:), allocatable, intent(in) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        read(state_str , *) code_state
        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: RESTORE STATE called'
        write(*,*) 'STATE TO BE RESTORED :', code_state
        write(*,*) '======================================='

    end subroutine restore_code_state

end module mod_basic
