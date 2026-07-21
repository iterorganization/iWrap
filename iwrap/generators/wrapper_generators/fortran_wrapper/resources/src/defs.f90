module iwrap_defs
    use iso_c_binding
    use ids_types, only: ids_string_length
    implicit none

!--------------------------------------------------
    integer, parameter :: AL_STRING_SIZE = ids_string_length
    integer, parameter :: AL_URI_SIZE    = 4096

    type, BIND(C) :: ids_description_t
        character(kind=c_char) :: ids_name(AL_STRING_SIZE)
        integer(c_int)         :: occurrence
        character(kind=c_char) :: uri(AL_URI_SIZE)
    end type ids_description_t
!--------------------------------------------------
end module
