
!--------------------------------------------------
module idsmodule
    type, BIND(C)::idsstruct
        character(132)::ids
        integer::shot
        integer::run
        integer::occurrence
        integer::idx
        character(132)::machine
        character(132)::user
        character(132)::version
    end type
end module
!--------------------------------------------------



!=====================Interface of user sbrt =================================
module user_sbrt_interface
interface
   subroutine physics_ii(in_equilibrium0, out_equilibrium1, imas_code_params, status_flag, status_message) 
      use ids_schemas
      implicit none
      
	! ----- Subroutine IN/OUT arguments ---------
	! ----- User defined arguments ---------
	type (ids_equilibrium)	::	in_equilibrium0
	type (ids_equilibrium)	::	out_equilibrium1

	 !----  Code parameters ----
	 type(ids_parameters_input) :: imas_code_params 
	! --------------------------------------------------------

    !----  Status info  ----
    integer, intent(out) :: status_flag
    character(len=:), pointer, intent(out) :: status_message

   end subroutine

 
end interface
end module 
!=============================================================================

module wrapper

contains

subroutine physics_iiual(M_in_equilibrium0, M_out_equilibrium1, code_params, status_info) BIND(C)
use idsmodule
use codeparam_module
use status_module
use ids_schemas
use iwrap_tools
use ids_routines
use user_sbrt_interface 
implicit none
character(132):: tabchar
integer     :: i,j,iloopmax,sizexml,sizexsd,sizedefaultxml

logical     :: file_exist


    type(code_parameters_t) :: code_params
    type(status_t) :: status_info


	 type(idsstruct) :: M_in_equilibrium0
	 type(idsstruct) :: M_out_equilibrium1
	! --------------------------------------- 
	type (ids_equilibrium) :: in_equilibrium0
	type (ids_equilibrium) :: out_equilibrium1
	
	
	
	 !----  Code parameters ----
	 type(ids_parameters_input) :: imas_code_params 

	 !----  Diagnostic info  ----
	 integer(C_INT)	::	wrap_out_outputFlag 
	 integer(C_INT)	::	wrap_out_diagnosticInfo_len 
	 type(C_PTR)	::	wrap_out_diagnosticInfo_cRetPtr
	 character(len=:), pointer :: diagnosticInfo

	integer :: n

!--------- Get IDS : in_equilibrium0 ------------------------
if (M_in_equilibrium0%occurrence/=0) then
        write(tabchar,"(i0)") M_in_equilibrium0%occurrence
        tabchar=trim(M_in_equilibrium0%ids)//"/"//trim(tabchar)
else
        tabchar=trim(M_in_equilibrium0%ids)
endif
call ids_get(M_in_equilibrium0%idx,trim(tabchar),in_equilibrium0)

! ------------------ code parameters ----------------------------
imas_code_params = convert(code_params)


	 ! ------------- Input data conversion [ISO_C_BINDING] --------------

!!!!!!!!!!!!!!!!!! your routine !!!!!!!!!!!!!!!!
call  physics_ii(in_equilibrium0, out_equilibrium1, imas_code_params,  wrap_out_outputFlag, diagnosticInfo)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	 ! ------------ Output data conversion [ISO_C_BINDING] --------------
	 if(associated(diagnosticInfo)) then
		 wrap_out_diagnosticInfo_len = LEN_TRIM(diagnosticInfo)
		 wrap_out_diagnosticInfo_cRetPtr = convert_string2Cptr(diagnosticInfo)
	 else
		 wrap_out_diagnosticInfo_len = -1 
   endif
         status_info.message_size = wrap_out_diagnosticInfo_len
         status_info.message = wrap_out_diagnosticInfo_cRetPtr
         status_info.code = wrap_out_outputFlag

	 ! Return in case of 'soft' crash (diagnostic info < 0)  
	 if(wrap_out_outputFlag < 0 ) then
		 return 
	 endif 

   if (associated(out_equilibrium1%code%name)) then
      deallocate(out_equilibrium1%code%name)
   endif
   if (associated(out_equilibrium1%code%version)) then
      deallocate(out_equilibrium1%code%version)
   endif
   allocate(out_equilibrium1%code%name(1))
   allocate(out_equilibrium1%code%version(1))
   out_equilibrium1%code%name(1) = "physics_ii"
   out_equilibrium1%code%version(1) = ""

!--------- Put IDS slice: out_equilibrium1 ------------------------
if (M_out_equilibrium1%occurrence/=0) then
        write(tabchar,"(i0)") M_out_equilibrium1%occurrence
        tabchar=trim(M_out_equilibrium1%ids)//"/"//trim(tabchar)
else
        tabchar=trim(M_out_equilibrium1%ids)
endif


 call ids_put(M_out_equilibrium1%idx,tabchar,out_equilibrium1)

call ids_deallocate(out_equilibrium1)
call ids_deallocate(in_equilibrium0)

end

end module wrapper
