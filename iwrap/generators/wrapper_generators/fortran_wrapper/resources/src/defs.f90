module iwrap_defs
    use iso_c_binding

!--------------------------------------------------
    integer, parameter :: STRING_SIZE = 132

    type, BIND(C) :: ids_description_t
        character(kind=c_char)::ids_name(STRING_SIZE)
        integer::shot
        integer::run
        integer::occurrence
        integer::idx
        character(kind=c_char)::machine(STRING_SIZE)
        character(kind=c_char)::user(STRING_SIZE)
        character(kind=c_char)::version(STRING_SIZE)
    end type ids_description_t
!--------------------------------------------------
end module
