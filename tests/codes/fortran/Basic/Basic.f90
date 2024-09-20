module mod_basic

implicit none

    integer :: code_state = 1
    real :: timestamp = 0

    interface codeStep
    module procedure code_step_ids_no_parameters, code_step_only_parameters, code_step_ids_parameters
    module procedure code_step_only_parameters_legacy, code_step_ids_parameters_legacy
    end interface codeStep

    interface initCode
        module procedure init_no_ids_no_parameters, init_only_parameters, init_only_ids, init_ids_parameters
        module procedure init_only_parameters_legacy, init_ids_parameters_legacy
    end interface initCode

    interface cleanUp
        module procedure clean_up_no_ids_no_parameters, clean_up_only_parameters, &
                clean_up_only_ids, clean_up_ids_parameters
        module procedure clean_up_only_parameters_legacy, clean_up_ids_parameters_legacy
    end interface cleanUp

contains
    ! =================================================================================================================
    !                                           CODE STATE SUBROUTINES
    ! =================================================================================================================

    !
    !    RESTORE_CODE_STATE SUBROUTINE
    !
    subroutine restore_code_state (state_str, status_code, status_message)

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
        write(*,*) 'Basic: RESTORE STATE called'
        write(*,*) 'STATE TO BE RESTORED :', code_state
        write(*,*) '======================================='

    end subroutine restore_code_state

    !
    !    GET_CODE_STATE SUBROUTINE
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
        write(*,*) 'Basic: GET CODE STATE called'
        write(*,*) 'STATE is :', state_str
        write(*,*) '======================================='

    end subroutine get_code_state

    ! =================================================================================================================
    !                                           TIMESTAMP SUBROUTINES
    ! =================================================================================================================

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

    ! =================================================================================================================
    !                                           INIT SUBROUTINES
    ! =================================================================================================================

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine init_no_ids_no_parameters(status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none
        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call init_only_parameters (parameters_string, status_code, status_message)

    end subroutine init_no_ids_no_parameters


    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine init_only_parameters_legacy(al_code_parameters, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_core_profiles) :: core_profiles_in
        type (ids_distribution_sources) :: distribution_sources_out
        type(ids_parameters_input), intent(in) :: al_code_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        call init_ids_parameters_legacy (core_profiles_in, distribution_sources_out, &
                al_code_parameters, status_code, status_message)

    end subroutine init_only_parameters_legacy

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine init_only_parameters(parameters_string, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_core_profiles) :: core_profiles_in
        type (ids_distribution_sources) :: distribution_sources_out
        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        call init_ids_parameters (core_profiles_in, distribution_sources_out, &
                parameters_string, status_code, status_message)

    end subroutine init_only_parameters

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine init_only_ids (core_profiles_in, distribution_sources_out,  status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_core_profiles), intent(in) :: core_profiles_in
        type (ids_distribution_sources), intent(out) :: distribution_sources_out
        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call init_ids_parameters (core_profiles_in, distribution_sources_out, &
                parameters_string, status_code, status_message)

    end subroutine init_only_ids

    subroutine init_ids_parameters_legacy (core_profiles_in, distribution_sources_out, &
            al_code_parameters, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources, IDS_STRING_LENGTH
        implicit none
        type (ids_core_profiles), intent(in) :: core_profiles_in
        type (ids_distribution_sources), intent(out) :: distribution_sources_out
        type (ids_parameters_input), intent(in) :: al_code_parameters
        integer, intent(inout) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        character(:), allocatable :: parameters_string
        integer :: string_size

        string_size = IDS_STRING_LENGTH * size(al_code_parameters%parameters_value)

        allocate(character(string_size) :: parameters_string)
        parameters_string = TRANSFER(al_code_parameters%parameters_value, parameters_string)

        call init_ids_parameters (core_profiles_in, distribution_sources_out, &
                parameters_string, status_code, status_message)

    end subroutine init_ids_parameters_legacy
    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine init_ids_parameters (core_profiles_in, distribution_sources_out, &
            parameters_string, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_core_profiles), intent(in) :: core_profiles_in
        type (ids_distribution_sources), intent(out) :: distribution_sources_out
        character(:), allocatable :: parameters_string
        integer, intent(inout) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: i


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: INITIALISATION called'
        write(*,*) '---------------------------------------'

        distribution_sources_out%ids_properties%homogeneous_time = core_profiles_in%ids_properties%homogeneous_time

        allocate(distribution_sources_out%time(size(core_profiles_in%time)))

        write(0,*) 'Received size of input time from core_profiles_in : ', SIZE(core_profiles_in%time)

        ! Fill in the output IDS (Physical data)
        do i=1,size(core_profiles_in%time)
           ! Time : copy from input IDS
           distribution_sources_out%time(i) = core_profiles_in%time(i)  + 1.0
        enddo

        allocate(distribution_sources_out%code%name(1))   ! For a string of 132 characters max.
        distribution_sources_out%code%name(1)   = 'basic methods: INIT'
        allocate(distribution_sources_out%code%output_flag(size(core_profiles_in%time)))
        distribution_sources_out%code%output_flag(:) = 0

        write(*,*) '---------------------------------------'
        write(*,*) 'Basic methods: INITIALISATION ends'
        write(*,*) '======================================='

    end subroutine init_ids_parameters

    ! =================================================================================================================
    !                                           FINALIZE SUBROUTINES
    ! =================================================================================================================

    !
    !    FINALISATION SUBROUTINE
    !
    subroutine clean_up_ids_parameters(distribution_sources_in, core_profiles_out, &
            parameters_string, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources
        implicit none

        type (ids_distribution_sources), intent(IN) :: distribution_sources_in
        type (ids_core_profiles), intent(OUT) :: core_profiles_out
        character(:), allocatable :: parameters_string

        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: i


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        write(*,*) '======================================='
        write(*,*) 'Basic methods: FINALISATION called'
        write(*,*) '---------------------------------------'


        core_profiles_out%ids_properties%homogeneous_time = distribution_sources_in%ids_properties%homogeneous_time

        allocate(core_profiles_out%time(size(distribution_sources_in%time)))

        write(0,*) 'Received size of input time from core_profiles_in : ', SIZE(distribution_sources_in%time)

        ! Fill in the output IDS (Physical data)
        do i=1,size(distribution_sources_in%time)
           ! Time : copy from input IDS
           core_profiles_out%time(i) = distribution_sources_in%time(i)  + 1.0
        enddo

        allocate(core_profiles_out%code%name(1))   ! For a string of 132 characters max.
        core_profiles_out%code%name(1)   = 'basic methods: FINALIZE'
        allocate(core_profiles_out%code%output_flag(size(distribution_sources_in%time)))
        core_profiles_out%code%output_flag(:) = 0

        write(*,*) '---------------------------------------'
        write(*,*) 'Basic methods: FINALISATION ends'
        write(*,*) '======================================='

    end subroutine clean_up_ids_parameters


    subroutine clean_up_no_ids_no_parameters(status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none

        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call clean_up_only_parameters(parameters_string, status_code, status_message)
    end subroutine clean_up_no_ids_no_parameters


    subroutine clean_up_only_parameters(parameters_string, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_distribution_sources) :: distribution_sources_in
        type (ids_core_profiles) :: core_profiles_out
        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        call clean_up_ids_parameters (distribution_sources_in, core_profiles_out, &
                parameters_string, status_code, status_message)
    end subroutine clean_up_only_parameters

    subroutine clean_up_only_ids(distribution_sources_in, core_profiles_out, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources
        implicit none

        type (ids_distribution_sources), intent(IN) :: distribution_sources_in
        type (ids_core_profiles), intent(OUT) :: core_profiles_out
        character(:), allocatable :: parameters_string

        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call clean_up_ids_parameters(distribution_sources_in, core_profiles_out, &
                parameters_string, status_code, status_message)
    end subroutine clean_up_only_ids

    subroutine clean_up_ids_parameters_legacy(distribution_sources_in, core_profiles_out, &
            al_code_parameters, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_parameters_input, IDS_STRING_LENGTH
        implicit none

        type (ids_distribution_sources), intent(IN) :: distribution_sources_in
        type (ids_core_profiles), intent(OUT) :: core_profiles_out
        type(ids_parameters_input) :: al_code_parameters
        character(:), allocatable :: parameters_string
        integer :: string_size

        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        string_size = IDS_STRING_LENGTH * size(al_code_parameters%parameters_value)

        allocate(character(string_size) :: parameters_string)
        parameters_string = TRANSFER(al_code_parameters%parameters_value, parameters_string)

        call clean_up_ids_parameters(distribution_sources_in, core_profiles_out, &
                parameters_string, status_code, status_message)
    end subroutine clean_up_ids_parameters_legacy

    subroutine clean_up_only_parameters_legacy(al_code_parameters, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_parameters_input, IDS_STRING_LENGTH
        implicit none

        type (ids_distribution_sources) :: distribution_sources_in
        type (ids_core_profiles) :: core_profiles_out
        type(ids_parameters_input) :: al_code_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call clean_up_ids_parameters_legacy(distribution_sources_in, core_profiles_out, &
                al_code_parameters, status_code, status_message)

    end subroutine clean_up_only_parameters_legacy


    ! =================================================================================================================
    !                                           MAIN SUBROUTINES
    ! =================================================================================================================

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_step_no_ids_no_parameters(status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none

        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call code_step_only_parameters(parameters_string, status_code, status_message)
    end subroutine code_step_no_ids_no_parameters

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_step_only_parameters(parameters_string, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid
        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call code_step_ids_parameters(equilibrium_in, equilibrium_out, parameters_string, status_code, status_message)
    end subroutine code_step_only_parameters

    subroutine code_step_only_parameters_legacy(al_code_parameters, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid
        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        type(ids_parameters_input) :: al_code_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call code_step_ids_parameters_legacy(equilibrium_in, equilibrium_out, &
                al_code_parameters, status_code, status_message)
    end subroutine code_step_only_parameters_legacy

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_step_ids_no_parameters(equilibrium_in, equilibrium_out, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid
        use ids_routines, only: ids_copy
        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call code_step_ids_parameters(equilibrium_in, equilibrium_out, parameters_string, status_code, status_message)
    end subroutine code_step_ids_no_parameters

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_step_ids_parameters(equilibrium_in, equilibrium_out, parameters_string, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid
        use ids_routines, only: ids_copy
        use mod_parameters_utils

        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        character(:), allocatable :: parameters_string
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        integer :: ntimes = 1
        real :: multiplication_factor = 1

        ! INITIALISATION OF ERROR FLAG
        status_code = 0
        allocate(character(50):: status_message)

        ! READ PARAMETERS
        call extract_values(parameters_string, ntimes, multiplication_factor)

        ! INITIAL DISPLAY
        write(*,*) '======================================='
        write(*,*) 'START OF PHYSICS CODE'
        write(*,*) '---------------------------------------'

        ! CHECK IF INPUT IDS IS VALID
        if (ids_is_valid(equilibrium_in%ids_properties%homogeneous_time).eqv..True. &
            .and.size(equilibrium_in%time)>0) then

            ! COPY THE INPUT IDS IN THE OUTPUT IDS
            call ids_copy(equilibrium_in, equilibrium_out)

            ! MANDATORY FLAG (UNIFORM TIME HERE)
            equilibrium_out%ids_properties%homogeneous_time = 1
            allocate(equilibrium_out%code%name(1))   ! For a string of 132 characters max.

            equilibrium_out%code%name(1)   = 'EXAMPLE: STEP method'
            allocate(equilibrium_out%code%version(1))   ! For a string of 132 characters max.

            ! MODIFY OUTPUT FLAG
            allocate(equilibrium_out%code%output_flag(size(equilibrium_in%time)))
            equilibrium_out%code%output_flag(:) = equilibrium_in%time(1) * &
                ntimes * multiplication_factor * code_state

            status_message = 'STEP: OK'
        else
            ! ERROR IF THE CODE DOES NOT COMPLETE TO THE END
            status_code = -1
            status_message = 'Error in STEP: input IDS not valid'
        endif

        code_state = code_state + 1
        timestamp = timestamp + 10

        ! FINAL DISPLAY
        write(*,*) '---------------------------------------'
        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='

    end subroutine code_step_ids_parameters

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_step_ids_parameters_legacy(equilibrium_in, equilibrium_out, &
            al_code_parameters, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid, IDS_STRING_LENGTH
        use ids_routines, only: ids_copy

        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        type(ids_parameters_input) :: al_code_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        character(:), allocatable :: parameters_string
        integer :: string_size

        string_size = IDS_STRING_LENGTH * size(al_code_parameters%parameters_value)

        allocate(character(string_size) :: parameters_string)
        parameters_string = TRANSFER(al_code_parameters%parameters_value, parameters_string)

        call code_step_ids_parameters(equilibrium_in, equilibrium_out, parameters_string, status_code, status_message)

    end subroutine code_step_ids_parameters_legacy


end module mod_basic