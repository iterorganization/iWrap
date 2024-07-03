---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.16.0
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---
# Writing iWrap-Compatible `MPI` Code

```{admonition} Key takeaways
:class: tip

- iWrap-compatible MPI code must avoid direct `MPI_Init` and `MPI_Finalize` calls, relying instead on `MPI_Initialized` and `MPI_Finalized` checks to ensure proper initialization and finalization managed by iWrap.
- Special attention must be given to the role of `rank 0` in MPI, as it is responsible for gathering and storing output data, while input data is read by every process.
- iWrap currently supports only Fortran and C++ codes, as there are no recognized standards for wrapping Python and Java codes within the iWrap framework.
```

This example delves into the essential steps and considerations for creating `MPI` code that adheres to iWrap standards, enabling seamless integration and execution within iWrap workflows. The structure and key iWrap compatibility aspects will be demonstrated using C++ and Fortran code examples.

Currently, only `Fortran` and `C++` codes are iWrapable as there are no recognized standards for `Python` and `Java` codes

## iWrap-Compatible `MPI` Code Structure Example

`````{tab-set}
````{tab-item} C++
```{code-block} cpp
:caption: examples/cp2ds-mpi_cpp/native_code/cp2ds_mpi.cpp

#include <mpi.h>
#include "ALClasses.h"
// =======================================
//             MAIN
//=======================================
void cp2ds_mpi_cpp(const IdsNs::IDS::core_profiles& in_core_profiles,
                         IdsNs::IDS::distribution_sources& out_distribution_sources,
                         int& status_code, std::string& status_message)
{
    int was_mpi_initialized, was_mpi_finalized;
    int mpi_size, mpi_rank;
    int idsSize = -1;

    MPI_Initialized(&was_mpi_initialized);
    if (!was_mpi_initialized)
        MPI_Init(NULL, NULL);

    printf("Entering subroutine eq2dist\n");
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    int world_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    printf("INFO: Process <%d>, out of: %d\n", mpi_rank, mpi_size);


    status_code = 0;

    idsSize = in_core_profiles.time.extent(0);

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of cp2ds_mpi_cpp CPP";

    out_distribution_sources.time.resize(idsSize);




    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_distribution_sources.time(i) = 10000 * mpi_rank + in_core_profiles.time(i);
    }

    out_distribution_sources.ids_properties.homogeneous_time = 1;
    out_distribution_sources.code.name   = "coreprofiles2distsource_cpp";
    out_distribution_sources.code.version   = "1.0";
    out_distribution_sources.code.parameters = "my_code_specific_parameters";
    out_distribution_sources.code.output_flag = 0   ;


    printf("End of coreprofiles2distsource CPP\n");
    MPI_Finalized(&was_mpi_finalized);
    if (!was_mpi_finalized)
       MPI_Finalize();
}
```
````
````{tab-item} Fortran
```{code-block} fortran
:caption: examples/cp2ds-mpi/native/cp2ds-mpi.f90

module mod_cp2ds_mpi
contains
subroutine coreprofiles2distsource_mpi(coreprofilesin, distsourceout, error_flag, error_message)

    use mpi
    use ids_schemas

    implicit none

    integer,parameter :: DP=kind(1.0D0)

    type (ids_core_profiles) :: coreprofilesin
    type (ids_distribution_sources) :: distsourceout
    integer, intent(out) :: error_flag
    character(len=:), pointer, intent(out) :: error_message

    integer :: i
    integer :: mpi_world_size, mpi_rank, error

    write(0,*) 'Entering subroutine coreprofiles2distsource_mpi'

    call MPI_initiazed(was_mpi_initialized, ierr)
    if (.not. was_mpi_initialized)   call MPI_Init(ierr)

    call MPI_Comm_size ( MPI_COMM_WORLD, mpi_world_size, error )
    call MPI_Comm_rank ( MPI_COMM_WORLD, mpi_rank, error )


    write (*,*) 'Info: process <', mpi_rank, '> out of: ', mpi_world_size


    write(0,*) 'size of input IDS  = ',size(coreprofilesin%time)

      ! INITIALISATION OF ERROR FLAG
      error_flag = 0
      allocate(character(50):: error_message)
      error_message = 'Status info of coreprofiles2distsource'

    ! The output IDS  must be allocated with its number of time slices (1 for a single time slice physics module)
    ! Here we allocate the output IDS  to the same size as the input IDS (but this is not a general rule)
    allocate(distsourceout%time(size(coreprofilesin%time)))


    write(0,*) 'Received size of input time from coreprofilesin : ', SIZE(coreprofilesin%time)

    ! Fill in the output IDS (Physical data)
    do i=1,size(coreprofilesin%time)
       ! Time : copy from input IDS
       distsourceout%time(i) = 10000 * mpi_rank + coreprofilesin%time(i)
       ! THE TIME FIELD MUST BE FILLED (MANDATORY) in case of multiple time slice mode for the IDS;

    enddo

    distsourceout%ids_properties%homogeneous_time = 1

    allocate(distsourceout%code%name(1))   ! For a string of 132 characters max.
    distsourceout%code%name(1)   = 'coreprofiles2distsource_mpi'
    allocate(distsourceout%code%version(1))   ! For a string of 132 characters max.
    distsourceout%code%version(1)   = '1.0'
    allocate(distsourceout%code%parameters(1))   ! For a string of 132 characters max.
    distsourceout%code%parameters(1) = 'my_code_specific_parameters'
    allocate(distsourceout%code%output_flag(1))
    distsourceout%code%output_flag(1) = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure

    write(0,*) 'End Subroutine'

    call MPI_finalized(was_mpi_finalized, ierr)
    if (.not. was_mpi_finalized)   call MPI_Finalize(ierr)
    
    return
end subroutine
end module mod_cp2ds_mpi
````
`````

There are some crucial `MPI` rules for iWrap compatibility.

1. You should avoid direct `MPI_Init` and `MPI_Finalize` calls.

   iWrap manages the initialization and finalization of `MPI`. Please use `MPI_Initialized` to check if `MPI` was initialized and `MPI_Finalized` to check if `MPI` was finished. Do not call `MPI_Finalize` if `MPI_Finalized` returned false.
   `````{tab-set}
   ````{tab-item} Fortran
   ```{code-block} fortran
   :caption: Example of Fortran conditional

     !   ----  MPI initialisation ----
     call MPI_initiazed(was_mpi_initialized, ierr)
     if (.not. was_mpi_initialized)   call MPI_Init(ierr)

     !   ----  MPI Finalisation ----
     call MPI_finalized(was_mpi_finalized, ierr)
     if (.not. was_mpi_finalized)   call MPI_Finalize(ierr)
   ```
   ````
   ````{tab-item} C++
   ```{code-block} cpp
   :caption: Example of C++ conditional
   int was_mpi_initialized, was_mpi_finalized;
   
   //----  MPI initialisation ----
   MPI_Initialized(&was_mpi_initialized);
   if (!was_mpi_initialized)
       MPI_Init(NULL, NULL);

   //----  MPI Finalization ----
   MPI_Finalized(&was_mpi_finalized);
   if (!was_mpi_finalized)
      MPI_Finalize();
   ```
   ````
   `````

2. Respect the special role of `rank 0`.

   The wrapper that runs the code, launched in parallel, reads input data in every processes but writes data only in `rank 0` process. So the code shall gather in `rank 0` process all results that need to be stored as output.
