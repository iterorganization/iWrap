module mod_basic_methods

contains

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine init_code (core_profiles_in, distribution_sources_out, xml_parameters, status_code, status_message)
        use ids_schemas, only: ids_parameters_input, ids_core_profiles, ids_distribution_sources
        implicit none
        type (ids_core_profiles), intent(in) :: core_profiles_in
        type (ids_distribution_sources), intent(out) :: distribution_sources_out
        type(ids_parameters_input), intent(in) :: xml_parameters
        integer, intent(out) :: status_code
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
        allocate(distribution_sources_out%code%output_flag(1))
        distribution_sources_out%code%output_flag(1) = 0

        write(*,*) '---------------------------------------'
        write(*,*) 'Basic methods: INITIALISATION ends'
        write(*,*) '======================================='

    end subroutine init_code


    !
    !    FINALISATION SUBROUTINE
    !
    subroutine clean_up(distribution_sources_in, core_profiles_out, status_code, status_message)
        use ids_schemas, only: ids_core_profiles, ids_distribution_sources
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

        write(*,*) '---------------------------------------'
        write(*,*) 'Basic methods: FINALISATION ends'
        write(*,*) '======================================='

    end subroutine clean_up

    !
    !    MAIN SUBROUTINE
    !
    subroutine step(equilibrium_in, equilibrium_out, xml_parameters, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_parameters_input, ids_is_valid
        use ids_routines, only: ids_copy

        implicit none

        type(ids_equilibrium):: equilibrium_in,equilibrium_out
        type(ids_parameters_input) :: xml_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        ! INITIALISATION OF ERROR FLAG
        status_code = 0
        allocate(character(50):: status_message)

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

            equilibrium_out%code%version(1)   = '1.0'
            allocate(equilibrium_out%code%output_flag(1))
            equilibrium_out%code%output_flag(1) = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure

            status_message = 'STEP: OK'
        else
            ! ERROR IF THE CODE DOES NOT COMPLETE TO THE END
            status_code = -1
            status_message = 'Error in STEP: input IDS not valid'
        endif

        ! FINAL DISPLAY
        write(*,*) '---------------------------------------'
        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='

        end subroutine step
end module mod_basic_methods
