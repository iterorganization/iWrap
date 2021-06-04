
!--------------------------------------------------
module codeparam_module
use iso_c_binding
    type, BIND(C)::code_parameters_t
        type(C_PTR) :: params 
        integer     :: params_size
        type(C_PTR) :: schema 
        integer     :: schema_size
    end type
end module
!--------------------------------------------------

module status_module
use iso_c_binding
    type, BIND(C)::status_t
        integer     :: code
        type(C_PTR) :: message
    end type
end module

module iwrap_tools
implicit none 

 interface convert
   module procedure &
       convert_codeparams,&
       convert_array2string,&
	   convert_string2array
 end interface
 
  interface convert2Cptr
   module procedure &
       convert_string2Cptr
 end interface
 
contains


FUNCTION convert_codeparams(code_params)  RESULT (xmllib_code_params)    
    use iso_c_binding, ONLY: C_PTR
    use ids_schemas, ONLY: ids_parameters_input
    use codeparam_module
    implicit none


    type(code_parameters_t) :: code_params
    type(ids_parameters_input) :: xmllib_code_params

    type(C_PTR)      :: c_str_ptr  

    character(kind=C_CHAR), dimension(:), pointer :: f_char_arr 

    
    integer     :: iloopmax, string_size, sizexml,sizexsd
    
    !  xml parameters
    c_str_ptr = code_params.params
    string_size = code_params.params_size
    iloopmax=string_size/132
    if (mod(string_size,132)/=0) then
    iloopmax = iloopmax + 1
    endif
    allocate(xmllib_code_params%parameters_value(iloopmax))
    
    call C_F_POINTER(c_str_ptr, f_char_arr, (/string_size/))
    xmllib_code_params%parameters_value = transfer(f_char_arr, xmllib_code_params%parameters_value)
    
    if(mod(string_size,132)/=0) then
    xmllib_code_params%parameters_value(iloopmax)(mod(string_size,132)+1:132) = ' '
    endif
    
    !get xsd
    c_str_ptr = code_params.schema
    string_size = code_params.schema_size
    iloopmax=string_size/132
    if (mod(string_size,132)/=0) then
    iloopmax = iloopmax + 1
    endif
    allocate(xmllib_code_params%parameters_default(iloopmax))
    
    call C_F_POINTER(c_str_ptr, f_char_arr, (/string_size/))
    xmllib_code_params%parameters_default = transfer(f_char_arr, xmllib_code_params%parameters_default)
    
    if (mod(string_size,132)/=0) then
    xmllib_code_params%parameters_default(iloopmax)(mod(string_size,132)+1:132) = ' '
    endif
END FUNCTION convert_codeparams



FUNCTION convert_array2string(inArray)  RESULT (outString)    
implicit none
character, dimension(:), intent(IN) :: inArray
character(len=:), allocatable :: outString
integer :: i
integer :: strSize

strSize = SIZE(inArray)
allocate(character(strSize)::outString)


DO i = 1,SIZE(inArray)
   outString(i:i) = inArray(i)
END DO
END FUNCTION convert_array2string

! ------------------------

FUNCTION convert_string2array(inString)  RESULT (outArray) 
implicit none  
character(len=*), INTENT(IN) :: inString
character, dimension(:), pointer :: outArray
integer :: i
integer :: strSize

strSize = LEN(inString)
allocate(outArray(strSize))

DO i = 1,LEN(inString)
   outArray(i) = inString(i:i)
END DO
END FUNCTION convert_string2array

! ======================================================================================
! ======================================================================================

FUNCTION convert_string2Cptr(inString)  RESULT (outPtr)
use iso_c_binding
implicit none
character(len=:), pointer, INTENT(IN) :: inString
type(C_PTR):: outPtr
character, dimension(:), pointer :: strArray
integer :: i, strSize


strSize = 0
if (associated(inString)) then 
   strSize = LEN_TRIM(inString)
   allocate(strArray(strSize + 1))

   DO I = 1, strSize
      strArray(I) = inString(I:I)
   END DO
   strArray(strSize + 1) = C_NULL_CHAR
   outPtr = C_LOC(strArray(1))

endif
END FUNCTION convert_string2Cptr

! ------------------------




end module iwrap_tools


