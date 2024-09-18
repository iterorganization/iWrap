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

# Empowering Stateful Code Resumption: A Developer's Guide to `GET_STATE`, `SET_STATE` and `GET_TIMESTAMP`

```{admonition} Key takeaways
:class: tip

- iWrap's `GET_STATE` and `SET_STATE` methods, implemented by developers, enable restarts of stateful codes while `GET_TIMESTAMP` provides monitoring information.
- iWrap provides the framework, but developers' effective implementation of these methods ensures robust and efficient state management.
- Practical code examples in C++/Fortran demonstrate how to integrate `GET_STATE`, `SET_STATE` and `GET_TIMESTAMP` into stateful codes within iWrap.
```

## Introduction

In iWrap, the `GET_STATE` and `SET_STATE` methods empower developers to navigate the complexities of stateful code execution, while `GET_TIMESTAMP` allows to get insight on the temporal aspects. This article explores the mechanics of these methods and emphasizes the developer's crucial role in ensuring robust and efficient restarts.

Stateful codes, prevalent in numerous computational domains, maintain dynamic internal states that evolve throughout execution. However, interruptions, whether planned or unplanned, can necessitate code restarts. Conventional restarts often discard valuable intermediate results, incurring wasted resources and potential delays.

## Developer Roles in Dynamic State Resumption

In the realm of iWrap, dynamic state restarts unlock unparalleled resilience for stateful codes. This capability hinges upon the collaborative relationship between iWrap's framework and the code developer. Let's delve into the developer's pivotal role in empowering this functionality.

### The Symphony of `GET_STATE`, `SET_STATE` and `GET_TIMESTAMP`

Just like iWrap provides core methods like `INIT`, `MAIN`, and `FINALIZE`, developers are entrusted with crafting three additional methods: `GET_STATE`, `SET_STATE` and `GET_TIMESTAMP`

- `GET_STATE`: This method acts as a state capture mechanism, snapshotting the code's inner workings at a specific point in time. The resulting **string representation** serves as the foundation for potential future continuation.
- `SET_STATE`: Functioning as a state restoration tool, **this method utilizes the stringified data** from `GET_STATE` to meticulously reconstruct the internal state, facilitating a seamless continuation from the captured point.
- `GET_TIMESTAMP`: Provides insight into the current execution time, offering valuable information for monitoring and debugging purposes.

These methods can be used manually by the developer within the Python workflow, or they can be employed automatically by workflow frameworks like MUSCLE3, which will be discussed later in the tutorial

### Putting Theory into Practice: `C++` and `Fortran` Examples

This section delves into practical code examples showcasing the implementation of `GET_STATE`, `SET_STATE`, and `GET_TIMESTAMP` methods within iWrap-able `C++` and `Fortran` code.

#### Advanced Code

##### `GET_STATE` and `SET_STATE` Code Example

Here's a `C++` code example demonstrating a basic implementation of `GET_STATE` and `SET_STATE` methods within a simple physics code. A similar approach is used in the `Fortran` code example, which demonstrates the implementation of these methods within a Fortran module. The code initializes the status code, prints initial status messages, and performs a loop where it increments `code_state` 20 times. Finally, it sets a status message and prints the final status. The `code_state` can be set and retrieved by GET and SET state methods.

`````{tab-set}
````{tab-item} C++
```{code-block} cpp
:caption: examples/code_restart_cpp/native_code/code_restart.cpp
:emphasize-lines: 8,15,31

#include "ALClasses.h"
#include "code_restart.h"

// =======================================
//             GET STATE
//=======================================

int code_state = 0;

void get_code_state( std::string& state_out, int& status_code, std::string& status_message)
{
    status_code = 0;

    status_message = "INITIALISATION: OK";
    state_out = std::to_string(code_state);

    std::cout << "=======================================================" << std::endl;
    std::cout << "Code restart CPP: GET STATE called" << std::endl;
    std::cout << "STATE is : " << state_out << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             SET STATE
//=======================================
void restore_code_state( std::string state, int& status_code, std::string& status_message)
{
    status_code = 0;
    status_message = "FINALISATION: OK";

    code_state = std::stoi( state );
    std::cout << "=======================================================" << std::endl;
    std::cout << "Code lifecycle CPP: RESTORE STATE called" << std::endl;
    std::cout << "STATE TO BE RESTORED : " << code_state << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             MAIN
//=======================================
void code_restart(const IdsNs::IDS::equilibrium& in_equilibrium,
                     int& status_code, std::string& status_message)
{

    // INITIALISATION OF ERROR FLAG
    status_code = 0;
    
    // INITIAL DISPLAY
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  "START OF PHYSICS CODE" << std::endl;
    

    std::cout <<  "Starting from: " << code_state << std::endl;



    for (int i = 0; i < 20; i++)
    {
        // COMPUTATIONS
        code_state++;
    }


    std::cout <<  "Counting to : " << code_state << std::endl;

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of code_restart CPP";

    
    // FINAL DISPLAY
    std::cout <<  "END OF PHYSICS CODE" << std::endl;
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  " " << std::endl;
}
```
````
```` {tab-item} Fortran
```{code-block} fortran
:caption: examples/code_restart/native_code/code_restart.f90
:emphasize-lines: 3,62-63,89

module mod_code_restart

integer :: code_state = 0

contains

    !
    !    MAIN SUBROUTINE
    !
    subroutine code_restart(equilibrium_in, status_code, status_message)
        use ids_schemas, only: ids_equilibrium, ids_is_valid

        implicit none

        type(ids_equilibrium):: equilibrium_in
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message
        integer :: i

        ! INITIAL DISPLAY
        write(*,*) ' '
        write(*,*) '======================================='
        write(*,*) 'START OF PHYSICS CODE'

        ! INITIALISATION OF ERROR FLAG
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'code_restart: OK'

        write(*,*) 'Starting from: ', code_state

        do i = 1, 20
            ! COMPUTATIONS
            code_state = code_state + 1
        end do

        write(*,*) 'Counting to: ', code_state

        ! FINAL DISPLAY
        write(*,*) 'END OF PHYSICS CODE'
        write(*,*) '======================================='
        write(*,*) ' '

        end subroutine code_restart

    !
    !    INITIALISATION SUBROUTINE
    !
    subroutine get_code_state (state_str, status_code, status_message)

        implicit none
        character(len=:), allocatable, intent(out) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        allocate(character(50):: state_str)
        write(state_str,*) code_state



        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: GET CODE STATE called'
        write(*,*) 'STATE is :', state_str
        write(*,*) '======================================='

    end subroutine get_code_state

    subroutine restore_code_state (state_str, status_code, status_message)
    !
    !    INITIALISATION SUBROUTINE
    !
        implicit none
        character(len=:), allocatable, intent(in) :: state_str
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message


        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

        read(state_str , *) code_state
        write(*,*) '======================================='
        write(*,*) 'Code lifecycle: RESTORE STATE called'
        write(*,*) 'STATE TO BE RESTORED :', code_state
        write(*,*) '======================================='

    end subroutine restore_code_state

end module mod_code_restart

```
````
`````

The provided `C++` and `Fortran` code demonstrates a basic implementation of `GET_STATE` and `SET_STATE` methods, capturing and restoring code state as a simple integer via global variable. This integer is passed as a string between these two methods and it's required that `SET_STATE` can understand string data given by `GET_STATE`.

##### `GET_TIMESTAMP` Code Example

Here's a `C++` and `Fortran` code example demonstrating a basic implementation of the `GET_TIMESTAMP` method

`````{tab-set}
````{tab-item} C++
```{code-block} cpp
:caption: examples/timestamps_cpp/native_code/timestamps.cpp
:emphasize-lines: 3,7,39

#include "ALClasses.h"

double timestamp = 0;

void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message)
{
    timestamp_out = timestamp;
}

void timestamps_cpp(const IdsNs::IDS::core_profiles& in_core_profiles,
                                       IdsNs::IDS::distribution_sources& out_distribution_sources,
                                       int& status_code, std::string& status_message)
{
    int idsSize = -1;

    printf("Entering subroutine timestamps_cpp\n");
    status_code = 0;

    idsSize = in_core_profiles.time.extent(0);

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of timestamps_cpp CPP";

    out_distribution_sources.time.resize(idsSize);



    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_distribution_sources.time(i) = in_core_profiles.time(i);
    }

    out_distribution_sources.ids_properties.homogeneous_time = 1;

    timestamp ++;

    printf("End of timestamps_cpp \n");

}
```
````
```` {tab-item} Fortran
```{code-block} fortran
:caption: examples/timestamps/native/timestamps.f90
:emphasize-lines: 3,19,67

MODULE mod_timestamps

    real :: timestamp = 0

contains


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                                   GET TIMESTAMP SUBROUTINE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
subroutine get_timestamp(timestamp_out, error_flag, error_message)

    integer,parameter :: DP=kind(1.0D0)
    real(kind=DP), intent(out) :: timestamp_out
    !----  Status info  ----
    integer, intent(out) :: error_flag
    character(len=:), pointer, intent(out) :: error_message

    timestamp_out = timestamp

end subroutine get_timestamp

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                                   MAIN SUBROUTINE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
subroutine timestamps(coreprofilesin, distsourceout, error_flag, error_message)

use ids_schemas

implicit none

integer,parameter :: DP=kind(1.0D0)

type (ids_core_profiles) :: coreprofilesin
type (ids_distribution_sources) :: distsourceout
integer, intent(out) :: error_flag
character(len=:), pointer, intent(out) :: error_message

integer :: i

write(0,*) 'Entering subroutine eq2dist'

write(0,*) 'size of input IDS  = ',size(coreprofilesin%time)

  ! INITIALISATION OF ERROR FLAG
  error_flag = 0
  allocate(character(50):: error_message)
  error_message = 'Status info of coreprofiles2distsource'

! The output IDS  must be allocated with its number of time slices (1 for a single time slice physics MODULE)
! Here we allocate the output IDS  to the same size as the input IDS (but this is not a general rule)
allocate(distsourceout%time(size(coreprofilesin%time)))


write(0,*) 'Received size of input time from equilibrium : ', SIZE(coreprofilesin%time)

! Fill in the output IDS (Physical data)
do i=1,size(coreprofilesin%time)
   ! Time : copy from input IDS
   distsourceout%time(i) = coreprofilesin%time(i)  
   ! THE TIME FIELD MUST BE FILLED (MANDATORY) in case of multiple time slice mode for the IDS;
    
enddo

distsourceout%ids_properties%homogeneous_time = 1

timestamp = timestamp + 1

write(0,*) 'End Subroutine'

return
end subroutine

end MODULE mod_timestamps
```
````
`````

These examples capture the essence of `GET_TIMESTAMP` implementation.

#### Advanced Code Description

##### `GET_STATE` and `SET_STATE` YAML Code Description

Moving on, you can use iWrap to create an actor from the yaml code description. When creating an actor, enter method signature (name) in yaml using `GET_STATE` and `SET_STATE` keys.

`````{tab-set}
````{tab-item} YAML
```{code-block} yaml
:caption: examples/code_restart_cpp/code_restart_cpp.yaml
:emphasize-lines: 5-6

code_description:
    implementation:
        subroutines:
            main:   code_restart
            get_state: get_code_state
            set_state: restore_code_state
        data_type: legacy
        code_path: examples/code_restart_cpp/native_code/libcode_restart_cpp.a
        include_path: examples/code_restart_cpp/native_code/code_restart.h
        programming_language: cpp
        data_dictionary_compliant: 3.37.0
    arguments:
    -   name: equilibrium_in
        type: equilibrium
        intent: IN
    documentation: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
        consequat. '
    settings:
        compiler_cmd: $CXX
        mpi_compiler_cmd:
        compiler_flags:
        extra_libraries:
            pkg_config_defined:
            path_defined:
```
````
`````

##### `GET_TIMESTAMP` YAML Code Description

Similar to `GET_STATE` and `SET_STATE`, the `GET_TIMESTAMP` method can be described in the YAML configuration file using the same key within the code_description section.

`````{tab-set}
````{tab-item} YAML
```{code-block} yaml
:caption: examples/timestamps_cpp/timestamps_cpp.yaml
:emphasize-lines: 12

actor_description:
    actor_name: timestamps_cpp
    actor_type: python
    data_type: legacy

code_description:
    implementation:
        subroutines:
            init:
            main:   timestamps_cpp
            finalize:
            get_timestamp: get_timestamp_cpp
        programming_language: cpp
        data_dictionary_compliant: 3.37.0
        data_type: legacy
        code_path: ./native_code/libtimestamps_cpp.a
        include_path: ./native_code/timestamps.h
        code_parameters:
            parameters:
            schema:
    arguments:
    -   name: core_profiles_in
        type: core_profiles
        intent: IN
    -   name: distribution_sources_out
        type: distribution_sources
        intent: OUT
    documentation: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
        consequat. '
    settings:
        compiler_cmd: $CXX
        mpi_compiler_cmd:
        compiler_flags:
        extra_libraries:
            pkg_config_defined:
            path_defined:
```
````
`````
