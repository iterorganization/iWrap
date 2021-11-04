module iwrap_defs
    use iso_c_binding

!--------------------------------------------------
    type, BIND(C)::code_parameters_t
        type(C_PTR) :: params  = C_NULL_PTR
        integer     :: params_size = 0
    end type

!--------------------------------------------------
    type, BIND(C)::status_t
        integer     :: code = 0
        type(C_PTR) :: message = C_NULL_PTR
    end type

!--------------------------------------------------
    type, BIND(C) :: ids_description_t
        character(132)::ids_name
        integer::shot
        integer::run
        integer::occurrence
        integer::idx
        character(132)::machine
        character(132)::user
        character(132)::version
    end type ids_description_t
!--------------------------------------------------
end module
