

program standalone
   use iso_c_binding
   use ids_schemas
   use ids_routines
   use idsmodule
   use rwtool
   use iwrap_tools
   use codeparam_module
   use wrapper
   implicit none
   character(len=5)::treename='ids'
   integer :: idx
   integer :: iArraySize 
!---------------------------------------------------------

	 type (ids_equilibrium) :: in_equilibrium0
	 type (idsstruct)::M_in_equilibrium0
	 type (ids_equilibrium) :: out_equilibrium1
	 type (idsstruct)::M_out_equilibrium1

    integer,parameter :: sizestring=90000
    character(sizestring)   :: codeparam
    character(sizestring)   :: codeparamxsd 
    character(sizestring)   :: defaultcodeparam
    integer           :: sizexml,sizexsd,sizedefaultxml

    type(code_parameters_t) :: code_params
    
   character(kind=C_CHAR), dimension(:), pointer   :: codeparam_c
   character(kind=C_CHAR), dimension(:), pointer   :: codeparamxsd_c 
   character(kind=C_CHAR), dimension(:), pointer   :: defaultcodeparam_c
   integer(kind=C_INT)          :: sizexml_c,sizexsd_c,sizedefaultxml_c
   integer :: i,j 
   integer, parameter :: nidss = 2 
   type(idsstruct), dimension(nidss) :: lidss 

	 !----  Diagnostic info  ----
	 integer         :: out_outputFlag = 0
	 character, dimension(:), pointer :: out_diagnosticInfoArray
	 character(len=:), allocatable :: out_diagnosticInfoString
	 integer  :: out_diagnosticInfo_size 
	 type(C_PTR)  :: out_diagnosticInfo_cptr 

	!-----------Reading input data from file ---------------------

   open(10,file='input.txt',form='formatted',access='sequential',status='old')
   
   call readfile(lidss(1))
   call readfile(lidss(2))
   codeparam = achar(0)
   codeparamxsd = achar(0)
   defaultcodeparam = achar(0)
   call readfile(sizexml)
   call readfile(codeparam, sizexml)
   call readfile(sizexsd)
   call readfile(codeparamxsd,sizexsd)
   call readfile(sizedefaultxml) 
   call readfile(defaultcodeparam,sizedefaultxml)
   close(10)

  
   !-----------translation of code param -----------------------------
   sizexml_c = LEN_TRIM(codeparam)
   allocate(codeparam_c(sizexml_c))
   do i=1, sizexml_c
      codeparam_c(i) = codeparam(i:i)
   end do

   sizexsd_c = LEN_TRIM(codeparamxsd)
   allocate(codeparamxsd_c(sizexsd_c))
   do i=1, sizexsd_c
      codeparamxsd_c(i) = codeparamxsd(i:i)
   end do

   sizedefaultxml_c = LEN_TRIM(defaultcodeparam)
   allocate(defaultcodeparam_c(sizedefaultxml_c))
   do i=1, sizedefaultxml_c
      defaultcodeparam_c(i) = defaultcodeparam(i:i)
   end do

    code_params.params =  C_LOC(codeparam_c(1))
    code_params.params_size =  sizexml_c

    code_params.schema =  C_LOC(codeparamxsd_c(1))
    code_params.schema_size =  sizexsd_c

    code_params.def_params =  C_LOC(defaultcodeparam_c(1))
    code_params.def_params_size =  sizedefaultxml_c
!---------------------------------------


   lidss(:)%idx = -1
   do i=1,nidss
      j=1
      do while (j.lt.i)
         if ( lidss(j)%shot .eq. lidss(i)%shot .and.       &
              lidss(j)%run .eq. lidss(i)%run .and.         &
              lidss(j)%user .eq. lidss(i)%user .and.       &
              lidss(j)%machine .eq. lidss(i)%machine .and. &
              lidss(j)%version .eq. lidss(i)%version ) then
            EXIT
         else
            j=j+1
         end if
      end do
      if (j.eq.i) then
         call imas_open_env(treename,lidss(i)%shot,lidss(i)%run,&
              lidss(i)%idx,lidss(i)%user,lidss(i)%machine,lidss(i)%version)
      else
         lidss(i)%idx = lidss(j)%idx
      end if
   end do
   M_in_equilibrium0 = lidss(1)
   M_out_equilibrium1 = lidss(2)

   !!!!!!!!! Fortran wrapper !!!!!!!!!!!!!!!
   call physics_iiual(M_in_equilibrium0, M_out_equilibrium1,  code_params, out_outputFlag, out_diagnosticInfo_size, out_diagnosticInfo_cptr)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if((.NOT. c_associated(out_diagnosticInfo_cptr)) .OR. (out_diagnosticInfo_size < 1)) then
      allocate(out_diagnosticInfoArray(23))
      out_diagnosticInfo_size = 23 
      out_diagnosticInfoArray = transfer("<No diagnostic message>", out_diagnosticInfoArray)
   else
      call c_f_pointer(out_diagnosticInfo_cptr, out_diagnosticInfoArray, [out_diagnosticInfo_size])
   endif
   print *, "---Diagnostic information returned from ***diagnostic***:---"
   print *, "-------Output flag    : ", out_outputFlag
   print *, "-------Diagnostic info: ", out_diagnosticInfoArray
   print *, "---------------------------------------------------------"
   out_diagnosticInfoString = convert (out_diagnosticInfoArray)

	!-----------Writing output data to file ---------------------
   open(10,file='output.txt',form='formatted',access='sequential',status='unknown')
   call writefile(out_outputFlag)
   call writefile(out_diagnosticInfo_size)
   call writefile(out_diagnosticInfoString,out_diagnosticInfo_size)
   close(10)

   do i=1,nidss
      j=1
      do while (j.lt.i)
         if (lidss(j)%idx.eq.lidss(i)%idx) then 
            EXIT
         else
            j=j+1
         end if
      end do
      if (j.eq.i) then 
         call imas_close(lidss(i)%idx)
      end if
   end do
   end program
