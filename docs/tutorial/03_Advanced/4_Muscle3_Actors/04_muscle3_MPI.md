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

# iWrap's MUSCLE3 MPI Example

```{admonition} Key takeaways
:class: tip

- iWrap facilitates the creation of MPI-enabled C++ actors, allowing for efficient parallel computation and seamless integration into MUSCLE3 workflows.
- The process involves defining the C++ actor's methods and packaging the code into a static library, followed by creating an actor description in YAML for iWrap to generate the actor.
- Proper configuration of the yMMSL file ensures smooth execution of the simulation by specifying the model components, conduits, settings, implementations, and resources.
```

In this chapter, we will delve into creating an MPI example utilizing iWrap. We will describe the process of using iWrap to generate a MUSCLE3-cpp actor type. This example will extend the previous work by incorporating MPI for parallel computations.

## Reusing the Macro Code

We will reuse [MUSCLE3 macro code from the previous chapter](03_MUSCLE3_actor_generation.md#example-of-a-macro-model-in-fortran) for this example. This macro code serves as the framework for the simulation, handling the coordination of various components.

## Creating the Micro Code in C++

In this example, we will create an iWrap-compatible `C++` actor. Let's start by examining the header file:

```{code-block} cpp
:caption: parallel_mpi.h

#ifndef _PARALLEL_MPI_CPP
#define _PARALLEL_MPI_CPP

#include "ALClasses.h"

void init_code (int& status_code, std::string& status_message);

void clean_up( int& status_code, std::string& status_message);

void code_step(const IdsNs::IDS::core_profiles& core_profiles_in,
                    IdsNs::IDS::distribution_sources& distribution_sources_out,
                    int& status_code, std::string& status_message);

void get_code_state( std::string& state_out, int& status_code, std::string& status_message);

void restore_code_state( std::string state, int& status_code, std::string& status_message);

void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message);

#endif // _PARALLEL_MPI_CPP
```

Next, let's look at the implementation in the `.cpp` file:

```{code-block} cpp
:caption: parallel_mpi.cpp

#include <mpi.h>
#include "parallel_mpi.h"

int code_state = 0;

// =======================================
//             INITIALISATION
//=======================================

void init_code (int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;

    status_code = 0;
    status_message = "INITIALISATION: OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: INITIALISATION called <" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             FINALISATION
//=======================================
void clean_up( int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;

    status_code = 0;
    status_message = "FINALISATION: OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: FINALISATION called <" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             MAIN
//=======================================
void code_step(const IdsNs::IDS::core_profiles& in_core_profiles,
                         IdsNs::IDS::distribution_sources& out_distribution_sources,
                         int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    int idsSize = -1;

    status_code = 0;
    status_message = "STEP OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout <<  "=======================================" << std::endl;
    std::cout <<  "START OF PHYSICS CODE<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout <<  "Starting from: " << code_state << std::endl;

    for (int i = 0; i < 20; i++)
    {
        // ANY COMPUTATIONS
        code_state++;
    }

    std::cout <<  "Counting to : " << code_state << std::endl;

    idsSize = in_core_profiles.time.extent(0);
    std::cout <<  "Size of input IDS: " <<idsSize << " <"<< mpi_rank << "/" << mpi_size << ">" << std::endl;

    if (idsSize > 0) {
        out_distribution_sources.time.resize(idsSize);
            // Fill in the output IDS (Physical data)
        for(int i=0; i < idsSize; i++)
        {
            // Time : copy from input IDS
            out_distribution_sources.time(i) =  1000000 * mpi_rank + 100 * code_state + in_core_profiles.time(i);
        }
    }
    else {
            out_distribution_sources.time.resize(1);
            out_distribution_sources.time(1) = 1000000 * mpi_rank + 100 * code_state;
    }
    out_distribution_sources.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;
    out_distribution_sources.code.name   = "cp2ds_mpi_cpp";
    out_distribution_sources.code.version   = "1.0";
    out_distribution_sources.code.output_flag = 0   ;


    std::cout <<  "END OF PHYSICS CODE" << std::endl;
    std::cout <<  "=======================================" << std::endl;

}

// =======================================
//             GET STATE
//=======================================


void get_code_state( std::string& state_out, int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    status_code = 0;

    status_message = "INITIALISATION: OK";
    state_out = std::to_string(code_state);

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: GET STATE called<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "STATE is : " << state_out << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             SET STATE
//=======================================
void restore_code_state( std::string state, int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    status_code = 0;
    status_message = "FINALISATION: OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    code_state = std::stoi( state );
    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: RESTORE STATE called<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "STATE TO BE RESTORED : " << code_state << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             GET TIMESTAMP
//=======================================
void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    timestamp_out = (double) code_state;

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: GET TIMESTAMP called<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "TIMESTAMP : " << timestamp_out << std::endl;
    std::cout << "=======================================================" << std::endl;
}
```

### Packaging the Code

To create a static library from the C++ code, use the `ar` utility. The following command compiles the code into an object file and then creates a static library:

```{code-block} sh
mpic++ -pthread -g -O0 -fPIC `pkg-config --libs --cflags al-cpp` -c -o parallel_mpi.o parallel_mpi.cpp
ar -cr libparallel_mpi.a parallel_mpi.o
```

### iWrap actor description

Next, we provide a code description for iWrap in a YAML file:

```{code-block} yml
:caption: mpi-cpp.yml

code_description:
    implementation:
        subroutines:
            init: init_code
            main:
                name: code_step
                arguments:
                -   name: core_profiles_in
                    type: core_profiles
                    intent: IN
                -   name: distribution_sources_out
                    type: distribution_sources
                    intent: OUT
            finalize: clean_up
            get_state: get_code_state
            set_state: restore_code_state
            get_timestamp: get_timestamp_cpp
        programming_language: cpp
        data_dictionary_compliant: 3.39.0
        data_type: legacy
        code_path: ./libparallel_mpi.a
        include_path: ./parallel_mpi.h
    documentation: 'Simple actor'
    settings:
        compiler_cmd: g++
        mpi_compiler_cmd: mpic++
        compiler_flags: -pthread
        extra_libraries:
            pkg_config_defined:
              - al-cpp
```

Using this actor description we can create an actor by calling:

```{code-block} sh
iwrap --actor-type muscle3-cpp -a mpi_actor -f mpi-cpp.yml
```

This actor will be available in `IWRAP_ACTORS` folder.

## Writing the yMMSL File

Next, create a yMMSL file that describes the simulation:

```{code-block} yaml
:caption: mpi-cpp.ymmsl

model:
  name: helloworld
  components:
    macro: macro
    micro: micro
  conduits:
    macro.core_profiles_out: micro.core_profiles_in
    micro.distribution_sources_out: macro.distribution_sources_in

settings:
  t_max: 4.0
  dt: 1.0
  muscle_remote_log_level: DEBUG

implementations:
  macro:
    executable: /pfs/work/g2bpogo/iwrap-muscle/macro.exe
  micro:
    executable: /pfs/work/g2bpogo/IWRAP_ACTORS/mpi_actor/bin/mpi_actor.exe

resources:
  macro:
    threads: 1
  micro:
    threads: 1
```

## Running simulation

To run the simulation, execute the following command:

```{code-block} sh
muscle_manager --start-all --log-level DEBUG mpi-cpp.ymmsl
```

After running this command, a test.out file will be created with the results of the simulation.