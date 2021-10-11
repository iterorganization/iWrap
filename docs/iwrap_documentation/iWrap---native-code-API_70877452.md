


Scientific Worfklows : iWrap - native code API








1. [Scientific Worfklows](index.md)
2. [Wrapping user codes into actors - iWrap](Wrapping-user-codes-into-actors---iWrap_70877391.md)





 Scientific Worfklows : iWrap - native code API
=================================================





 
 
 
 
 
 
 
 Created by  Bartosz Palak, last modified on 10 wrz 2021
 




* [1. Introduction](#iWrapnativecodeAPI-Introduction)

* [2. Fortran](#iWrapnativecodeAPI-Fortran)

	+ [2.1. Native code signature](#iWrapnativecodeAPI-Nativecodesignature)
	+ [2.2. Module](#iWrapnativecodeAPI-Module)
	+ [2.3. Subroutine](#iWrapnativecodeAPI-Subroutine)
	+ [2.4. Arguments](#iWrapnativecodeAPI-Arguments)
	+ [2.5. Example](#iWrapnativecodeAPI-Example)
* [3.  C++](#iWrapnativecodeAPI-C++)

	+ [3.1. Native code signature](#iWrapnativecodeAPI-Nativecodesignature.1)
	+ [3.2. Header](#iWrapnativecodeAPI-Header)
	+ [3.3. Method](#iWrapnativecodeAPI-Method)
	+ [3.4. Arguments](#iWrapnativecodeAPI-Arguments.1)
	+ [3.5. Example](#iWrapnativecodeAPI-Example.1)
* [4. MPI](#iWrapnativecodeAPI-MPI)

* [5. Code packaging](#iWrapnativecodeAPI-Codepackaging)




1.Introduction
===============

A signature of user code must follow strict rules to allow interaction between it and wrapping actor. Without the detailed knowledge of method signature iWrap cannot built an actor.

* Only IDS data are accepted as dynamic data input/output

* Static data provided as additional codeparam input ,

* Optional error code/mesg output (not mandatory),

* A constraint: at least one IDS is either produced or consumed


  








| 


|  |

 |


  


* Input and output IDSes:

	+ **Optional** arguments
	+ Intent: IN or OUT
* XML parameters:

	+ **Optional** argument
	+ Intent: IN
* Status code:

	+ **Mandatory** argument
	+ Intent: OUT
* Status message

	+ **Mandatory** argument
	+ Intent: OUT

The current version of iWrap allows to wrap native code implemented in Fortran or C++

  


  


Important!

A native code that will be wrapped by iWrap and that will become a part of workflow should be compiled using the same environment in which workflow will be run!

  



2.Fortran
==========

2.1. Native code signature
--------------------------

  




`
module <module name>
  
subroutine <subroutine name> ([ids1, ids2, ..., idsN,] [xml_parameters], status_code, status_message)
  use ids_schemas
  ! IN/OUT IDSes
  type(ids_<ids_name>), intent([IN|OUT]):: ids1
  type(ids_<ids_name>), intent([IN|OUT]):: ids2
   . . .
  type(ids_<ids_name>), intent([IN|OUT]):: idsN 
 
 
  ! XML code parameters
  type(ids_parameters_input) :: xml_parameters
 
  ! status info
  integer, intent(OUT) :: status_code
  character(len=:), pointer, intent(OUT) :: status_message
 
end subroutine <subroutine name>
end module <module name>
`

  


2.2. Module
-----------

* Native code should be put within a module

* Module is used by compiler to check, if code signature expected by wrapper is exactly the same as provided.

* A name of the module could be arbitrary - chosen by code developer


2.3. Subroutine
---------------

* A user code should be provided as a subroutine (and not a function)

* A name of the module could be arbitrary - chosen by code developer


2.4. Arguments
--------------

Arguments shall be provided in a strict order:

* Input and output IDSes:

	+ **Optional** arguments
	+ Intent: IN or OUT
	+ Defined as "`type(ids_<ids_name>)` "
* XML parameters:

	+ **Optional** argument
	+ Intent: IN
	+ Defined as "`type(ids_parameters_input), intent(IN)"`
* Status code:

	+ **Mandatory** argument
	+ Intent: OUT
	+ Defined as  "`integer, intent(OUT)"`
* Status message

	+ **Mandatory** argument
	+ Intent: OUT
	+ Defined as: `character(len=:), pointer, intent(OUT)`

No INOUT arguments are allowed!

2.5. Example
------------



`
module physics_ii_mod
  
subroutine physics_ii(equilibrium_in, equilibrium_out, code_param, error_flag, error_message)      

  use ids_schemas   

  ! IN/OUT IDSes
  type(ids_equilibrium):: equilibrium_in, equilibrium_out
 
  ! XML code parameters
  type(ids_parameters_input) :: code_param
 
  ! status info
  integer, intent(out) :: error_flag
  character(len=:), pointer, intent(out) :: error_message
 
end subroutine physics_ii
end module physics_ii_mod
`


3. C++
=======

3.1. Native code signature
--------------------------

  




`
#include "UALClasses.h"
void <method name>([IdsNs::IDS::<ids_name> ids1, ..., IdsNs::IDS::<ids_name>& idsN,] [IdsNs::codeparam_t codeparam,] int* status_code, char** status_message)
`

3.2. Header
-----------

To generate an actor user has to provide a file containing C++ header of wrapped method. This file can be of arbitrary name but must contain method signature.

3.3. Method
-----------

* A user code should be provided as a single method

* A name of the module could be arbitrary - chosen by code developer


3.4. Arguments
--------------

Arguments shall be provided in a strict order:

* Input IDSes:

	+ **Optional** arguments
	+ Defined as `"IdsNs::IDS::<ids_name>"`
* Output IDSes:

	+ **Optional** arguments
	+ Defined as `IdsNs::IDS::<ids_name>&`  (please notice reference sign - '&')
* XML parameters:

	+ **Optional** argument
	+ Input argument
	+ Defined as `"IdsNs::codeparam_t` "
* Status code:

	+ **Mandatory** argument
	+ Output argument
	+ Defined as  `"int*"`
* Status message

	+ **Mandatory** argument
	+ Output argument
	+ Defined as: "`char**` "

No INOUT arguments are allowed!

3.5. Example
------------

**Header file - physics_ii.h**

`
#ifndef _LEVEL_II_CPP
#define _LEVEL_II_CPP

#include "UALClasses.h"

void physics_ii_cpp(IdsNs::IDS::equilibrium in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium, IdsNs::codeparam_t codeparam, int* status_code, char** status_message);

#endif // _LEVEL_II_CPP
`

**Implementation file - level_ii.cpp**

`
#include "UALClasses.h"
void physics_ii_cpp(IdsNs::IDS::equilibrium in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium, IdsNs::codeparam_t codeparam, int* status_code, char** status_message)
{
....
}
`


4.MPI
======

All native codes that use MPI should follow the rules described below:

* Please make initialisation and finalisation conditional, checking if such action was already made.




| `!----  MPI initialisation ----call MPI_initialized(was_mpi_initialized, ierr)if (.not. was_mpi_initialized)   call MPI_Init(ierr)` `!----  MPI Finalisation ----call MPI_finalized(was_mpi_finalized, ierr)if (.not. was_mpi_finalized)   call MPI_Finalize(ierr)` |

  


* Please be aware of a special role of ranked 0 process: Wrapper that run native code,  launched in parallel, reads input data in every processes but writes it only in 'rank 0' process. So native code should gather all results that need to be stored by 'rank 0' process. It concerns also those coming from 'rank 0' process are analysed by wrapper.



5.Code packaging
=================

A native code written in C++ or Fortran should be packed within static Linux library using e.g. `ar` tool for that purpose.



`
ar -cr lib<name>.a <object files *.o list>
e.g.:
ar -cr libphysics_ii.a *.o
`

  


  






Attachments:
------------




![](images/icons/bullet_blue.gif)

 


Document generated by Confluence on 27 wrz 2021 14:37


[Atlassian](http://www.atlassian.com/)


 
