module iwrap_converters
    use iwrap_defs
    use ISO_C_BINDING

    implicit none

    interface
        ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !                                   iwrap_C_strlen
        !
        !   Interface to publish C strlen function
        !
        ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        function iwrap_C_strlen(s) result(result) bind(C,name="strlen")
            use iso_c_binding

            integer(C_SIZE_T) :: result
            type(C_PTR), value, intent(in) :: s  !character(len=*), intent(in)
        end function
    end interface

contains

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   c_str_length
    !
    !   Helper function to add aditional checks to C 'strlen'
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    FUNCTION c_str_length(c_string_ptr) result(length)


        integer(C_SIZE_T) :: length
        type(C_PTR), value, intent(in) :: c_string_ptr

        if (.not. C_ASSOCIATED(c_string_ptr)) then
            length = 0
        else
            length = iwrap_C_strlen(c_string_ptr)
        end if
    end FUNCTION c_str_length

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   convert_array2codeparams
    !
    !   Function puts string, obtained from C/Py via ISO_C_BINDINGS to AL ids_parameters_input structure
    !
    !   FROM:  character(kind=C_CHAR), dimension(:) - string as array, obtained from C/Py via ISO_C_BINDINGS
    !   TO:    type(ids_parameters_input)         - Access Layer 'ids_parameters_input' structure
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    FUNCTION convert_array2codeparams(code_params_cstr)  RESULT (al_code_params)
        use ids_schemas, ONLY: ids_parameters_input

        character(kind=c_char), dimension(:), intent(IN) :: code_params_cstr
        type(ids_parameters_input) :: al_code_params

        al_code_params%parameters_value => convert_array2al_str(code_params_cstr)

    END FUNCTION convert_array2codeparams

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   convert_array2string
    !
    !   Function converts string, obtained from C/Py via ISO_C_BINDINGS to regular Fortran string
    !
    !   FROM:  character(kind=C_CHAR), dimension(:) - string as array, obtained from C/Py via ISO_C_BINDINGS
    !   TO:    character(len=:), allocatable        - regular Fortran string
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    FUNCTION convert_array2string(in_array)  RESULT (out_string)
        character(kind=C_CHAR), dimension(:), intent(IN) :: in_array
        character(len=:), allocatable :: out_string
        integer :: i
        integer(C_SIZE_T) :: str_length

        str_length = SIZE(in_array)
        allocate(character(str_length)::out_string)

        DO i = 1, str_length
           out_string(i:i) = in_array(i)
        END DO

    END FUNCTION convert_array2string

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   convert_string2array
    !
    !   Function converts regular Fortran string to 'ISO_C_BINDINGS style' string. The converted string is used
    !   in 'standalone' mode to mimic data passed to wrapper from Python actor in 'normal' mode
    !
    !   FROM:    character(len=*)                 - regular Fortran string
    !   TO:  character(kind=C_CHAR), dimension(:) - string as array, as used by ISO_C_BINDINGS
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    FUNCTION convert_string2array(in_string)  RESULT (out_array)
        character(len=*), INTENT(IN) :: in_string
        character, dimension(:), pointer :: out_array
        integer :: i
        integer :: str_length

        str_length = LEN(in_string)
        allocate(out_array(str_length))

        DO i = 1, str_length
           out_array(i) = in_string(i:i)
        END DO

    END FUNCTION convert_string2array


    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   convert_string2cptr
    !
    !   Function returns C pointer, poiting to the input Fortran string. The pointer is returned
    !   from the wrapper to Python actor
    !
    !   FROM:    character(len=:), pointer -  Fortran string
    !   TO:      type(C_PTR)               -  C pointer, pointing to the string
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    FUNCTION convert_string2cptr(in_string)  RESULT (out_cptr)

        character(len=*), INTENT(IN) :: in_string
        type(C_PTR) :: out_cptr
        INTEGER :: i, str_length
        character, dimension(:), pointer :: out_array

        str_length = LEN(in_string)
        allocate(out_array(str_length + 1))

        DO i = 1, str_length
           out_array(i) = in_string(i:i)
        END DO

        out_array(str_length + 1) =  C_NULL_CHAR
        out_cptr = C_LOC(out_array)

    END FUNCTION convert_string2cptr

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   convert_string2cptr
    !
    !   Function converts string pointed by C pointer to the regular Fortran string.
    !
    !  FROM:  type(C_PTR)                   -  C pointer, pointing to the input string
    !  TO:    character(len=:), allocatable -  Fortran string
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    FUNCTION convert_cptr2string(in_c_ptr) RESULT( out_string)

        type(C_PTR), intent(in) :: in_c_ptr
        character(len=:), allocatable :: out_string
        character(kind=C_CHAR), dimension(:), pointer :: char_arr
        integer :: str_length
        if (.not. C_associated(in_c_ptr)) then
            out_string = ''
            return
        endif

        str_length = c_str_length(in_c_ptr)
        call C_F_pointer(in_c_ptr, char_arr,  (/str_length/))

        allocate(character(str_length)::out_string)
        out_string = transfer(char_arr, out_string)

    end FUNCTION

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !                                   convert_array2al_str
    !
    !   Function converts string pointed by C pointer to the regular Fortran string.
    !
    !  FROM:  type(C_PTR)                   -  C pointer, pointing to the input string
    !  TO:    character(len=AL_STRING_SIZE), dimension(:) -  string in 'Access Layer standard'
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    FUNCTION convert_array2al_str(in_char_arr) RESULT (al_str)
        ! converts long string (character(len>132)) to array of strings no longer than 132 characters


        character(kind=c_char), dimension(:), intent(IN) :: in_char_arr
        character(len=AL_STRING_SIZE), dimension(:), pointer :: al_str(:)

        integer  :: rows
        integer  :: last_row_len
        integer  :: array_size


        array_size = SIZE(in_char_arr)
        rows = array_size / AL_STRING_SIZE
        last_row_len = mod(array_size, AL_STRING_SIZE)

        if (last_row_len /= 0) then
            rows = rows + 1
        endif
        allocate(al_str(rows))

        al_str = transfer(in_char_arr(1:array_size), al_str)

        if(last_row_len /= 0) then
            al_str(rows)((last_row_len + 1):AL_STRING_SIZE) = ' '
        endif

    END FUNCTION convert_array2al_str

end module iwrap_converters


