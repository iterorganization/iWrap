module mod_basic

implicit none

    type type_codeparam_physics_data
        integer :: ntimes
        double precision:: multiplication_factor
    end type type_codeparam_physics_data

    integer :: code_state = 0

    interface code_step
        module procedure codeStepIdsNoParameters, codeStepIdsParameters
    end interface code_step

    interface init_code
        module procedure initNoIdsNoParameters, initOnlyParameters, initIdsOnly, initIdsParameters
    end interface init_code

    interface clean_up
        module procedure cleanUpNoIds, cleanUpIds
    end interface clean_up

contains
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
    subroutine initNoIdsNoParameters(status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none
        type(ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call initOnlyParameters (xml_parameters, status_code, status_message)

    end subroutine initNoIdsNoParameters

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine initOnlyParameters(xml_parameters, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_core_profiles) :: core_profiles_in
        type (ids_distribution_sources) :: distribution_sources_out
        type(ids_parameters_input), intent(in) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        call initIdsParameters (core_profiles_in, distribution_sources_out, xml_parameters, status_code, status_message)

    end subroutine initOnlyParameters

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine initIdsOnly (core_profiles_in, distribution_sources_out,  status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_core_profiles), intent(in) :: core_profiles_in
        type (ids_distribution_sources), intent(out) :: distribution_sources_out
        type (ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call initIdsParameters (core_profiles_in, distribution_sources_out, xml_parameters, status_code, status_message)

    end subroutine initIdsOnly

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine initIdsParameters (core_profiles_in, distribution_sources_out, xml_parameters, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        use ids_routines, only: IDS_TIME_MODE_UNKNOWN
        implicit none
        type (ids_core_profiles), intent(in) :: core_profiles_in
        type (ids_distribution_sources), intent(out) :: distribution_sources_out
        type (ids_parameters_input), intent(in) :: xml_parameters
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


        distribution_sources_out%ids_properties%homogeneous_time = 1
        if (core_profiles_in%ids_properties%homogeneous_time /= IDS_TIME_MODE_UNKNOWN) then

           if (associated(core_profiles_in%time)) then
                allocate(distribution_sources_out%time(size(core_profiles_in%time)))

                write(0,*) 'Received size of input time from core_profiles_in : ', SIZE(core_profiles_in%time)

                ! Fill in the output IDS (Physical data)
                do i=1,size(core_profiles_in%time)
                   ! Time : copy from input IDS
                   distribution_sources_out%time(i) = core_profiles_in%time(i)  + 1.0
                enddo

           else
                allocate(distribution_sources_out%time(1))
                distribution_sources_out%time(1) = 1000 * code_state
           end if

            allocate(distribution_sources_out%code%name(1))   ! For a string of 132 characters max.
            distribution_sources_out%code%name(1)   = 'basic methods: INIT'
            allocate(distribution_sources_out%code%output_flag(1))
            distribution_sources_out%code%output_flag(1) = 0

        end if

        write(*,*) '---------------------------------------'
        write(*,*) 'Basic methods: INITIALISATION ends'
        write(*,*) '======================================='

    end subroutine initIdsParameters

    !
    !    FINALISATION SUBROUTINE
    !
    subroutine cleanUpNoIds(status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources
        implicit none

        type (ids_distribution_sources) :: distribution_sources_in
        type (ids_core_profiles)  :: core_profiles_out

        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call cleanUpIds(distribution_sources_in, core_profiles_out, status_code, status_message)

    end subroutine cleanUpNoIds

    !
    !    FINALISATION SUBROUTINE
    !
    subroutine cleanUpIds(distribution_sources_in, core_profiles_out, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources
        use ids_routines, only: IDS_TIME_MODE_UNKNOWN

        implicit none

        type (ids_distribution_sources), intent(IN) :: distribution_sources_in
        type (ids_core_profiles), intent(OUT) :: core_profiles_out

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



        if (distribution_sources_in%ids_properties%homogeneous_time /= IDS_TIME_MODE_UNKNOWN) then
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
            allocate(core_profiles_out%code%output_flag(1))
            core_profiles_out%code%output_flag(1) = 0

        end if

        write(*,*) '---------------------------------------'
        write(*,*) 'Basic methods: FINALISATION ends'
        write(*,*) '======================================='

    end subroutine cleanUpIds

    !
    !    MAIN SUBROUTINE
    !
    subroutine codeStepNoIdsNoParameters(status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        implicit none

        type(ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call codeStepNoIdsParameters(xml_parameters, status_code, status_message)
    end subroutine codeStepNoIdsNoParameters

    !
    !    MAIN SUBROUTINE
    !
    subroutine codeStepNoIdsParameters(xml_parameters, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_parameters_input
        implicit none

        type(ids_core_profiles):: core_profiles_in
        type(ids_distribution_sources):: distribution_sources_out
        type(ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call codeStepIdsParameters(core_profiles_in, distribution_sources_out, xml_parameters, status_code, status_message)
    end subroutine codeStepNoIdsParameters

    !
    !    MAIN SUBROUTINE
    !
    subroutine codeStepIdsNoParameters(core_profiles_in, distribution_sources_out, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_parameters_input
        implicit none

        type(ids_core_profiles):: core_profiles_in
        type(ids_distribution_sources):: distribution_sources_out
        type(ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        call codeStepIdsParameters(core_profiles_in, distribution_sources_out, xml_parameters, status_code, status_message)
    end subroutine codeStepIdsNoParameters

    !
    !    MAIN SUBROUTINE
    !
    subroutine codeStepIdsParameters(core_profiles_in, distribution_sources_out, xml_parameters, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_parameters_input
        use ids_routines, only: IDS_TIME_MODE_UNKNOWN

        implicit none

        type(ids_core_profiles), intent(IN) :: core_profiles_in
        type(ids_distribution_sources), intent(OUT) :: distribution_sources_out
        type(ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: i

        ! INITIALISATION OF ERROR FLAG
        status_code = 0
        allocate(character(50):: status_message)

        ! INITIAL DISPLAY
        write(*,*) '======================================='
        write(*,*) 'START OF PHYSICS CODE'
        write(*,*) '---------------------------------------'

        write(*,*) 'Starting from: ', code_state
        do i = 1, 20
            ! COMPUTATIONS
            code_state = code_state + 1
        end do

        write(*,*) 'Counting to: ', code_state

        ! CHECK IF INPUT IDS IS VALID
        if ( core_profiles_in%ids_properties%homogeneous_time /= IDS_TIME_MODE_UNKNOWN  &
            .and.size(core_profiles_in%time)>0) then


            ! MANDATORY FLAG (UNIFORM TIME HERE)
            distribution_sources_out%ids_properties%homogeneous_time = 1
            allocate(distribution_sources_out%code%name(1))   ! For a string of 132 characters max.

            distribution_sources_out%code%name(1)   = 'EXAMPLE: STEP method'
            allocate(distribution_sources_out%code%version(1))   ! For a string of 132 characters max.

            distribution_sources_out%code%version(1)   = '1.0'
            allocate(distribution_sources_out%code%output_flag(1))
            distribution_sources_out%code%output_flag(1) = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure

            ! Fill in the output IDS (Physical data)
            allocate(distribution_sources_out%time(size(core_profiles_in%time)))
            do i=1,size(core_profiles_in%time)
               ! Time : copy from input IDS
               distribution_sources_out%time(i) = 1000 * code_state + core_profiles_in%time(i)
            enddo

        endif

        status_message = 'STEP: OK'

        ! FINAL DISPLAY
        write(*,*) '---------------------------------------'
        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='

        end subroutine codeStepIdsParameters

end module mod_basic