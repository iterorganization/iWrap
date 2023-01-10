module mod_code_restart

integer :: code_state = 0

contains

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_restart(equilibrium_in, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_is_valid

        implicit none

        type(ids_equilibrium):: equilibrium_in
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: i

        ! INITIAL DISPLAY
        write(*,*) ' '
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

        ! FINAL DISPLAY
        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='
        write(*,*) ' '

        end subroutine code_restart

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

end module mod_code_restart
