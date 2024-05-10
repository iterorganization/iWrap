module iwrap_defs
    use iso_c_binding
    implicit none

!--------------------------------------------------
    integer, parameter :: AL_STRING_SIZE = 132

    type, BIND(C) :: ids_description_t
        character(kind=c_char)::ids_name(AL_STRING_SIZE)
        integer::shot
        integer::run
        integer::occurrence
        integer::idx
        character(kind=c_char)::machine(AL_STRING_SIZE)
        character(kind=c_char)::user(AL_STRING_SIZE)
        character(kind=c_char)::version(AL_STRING_SIZE)
    end type ids_description_t
!--------------------------------------------------
end module
