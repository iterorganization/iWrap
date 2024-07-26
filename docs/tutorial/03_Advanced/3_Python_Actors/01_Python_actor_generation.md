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
# Reminder: Generating Python Actors with iWrap

```{admonition} Key takeaways
:class: tip

- iWrap facilitates the seamless integration of custom Python actors into iWrap workflows by utilizing YAML configuration files, streamlining the actor generation process.
- The iWrap GUI offers a user-friendly interface for specifying actor details, generating actors, and providing real-time feedback during the generation process.
- The generated Python actor can be easily incorporated into existing Python workflows, allowing for efficient management of runtime settings, code parameters, and state restoration for iterative computations.
```

This serves as a concise reminder regarding the process of generating Python actors using iWrap configuration files. This approach streamlines the integration of your custom code into iWrap actors. For the most comprehensive instructions, consult [Code description and actor generation](../../02_Basic/1_Actor_Fundamentals/03_code_description_and_actor_generation.md) chapter.

## Actor Description

### YAML File

Create a `.yaml` file, specifying details like the `actor name`, arguments, and code implementation details. Adhere to the correct format and structure as outlined in the iWrap documentation. The example of `MPI` code from the chapter before will be used to show proper `yaml` format.

First, you need to package the code to a `*.a` library format

```{code-block} sh
mpic++ -pthread -g -O0 -fPIC `pkg-config --libs --cflags al-cpp` -c -o cp2ds_mpi.o cp2ds_mpi.cpp
ar -cr libcp2ds_mpi.a cp2ds_mpi.o
```

After a successful library creation, one can reference it in the `yaml` code description.

```{code-block} yaml
code_description:
    implementation:
        subroutines:
            main: cp2ds_mpi_cpp
        data_type: legacy
        programming_language: cpp
        data_dictionary_compliant: 3.37.0
        code_path: ./libcp2ds_mpi_cpp.a
        include_path: ./cp2ds_mpi.h
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
    documentation: 'MPI actor'
    settings:
        compiler_cmd: $CXX
        mpi_compiler_cmd: $MPICXX
```

```{admonition} MPI actors
:class: important

Be mindful that actor is considered an `MPI` actor if `mpi_compiler_cmd` is set.
```

### iWrap GUI

You can leverage the user-friendly graphical interface to effortlessly fill in the required information, ensuring accuracy and clarity.

## Actor Generation

### Command-Line Interface (CLI)

Execute the iwrap command with specific arguments, including the path to the YAML file and the desired actor type (python). Refer to the iWrap documentation for the exact syntax and usage examples.

```{code-block} sh
iwrap -a cp2ds_mpi --actor-type python -f code-description.yml
```

### iWrap GUI

Click the "Generate" button within the GUI. A separate window will display the generation logs, providing feedback on the process.

## Python Workflow

The generated Python actor file, now ready for use, will reside in the specified directory (typically within an iWrap-generated folder). Seamlessly integrate this actor into your Python workflow scripts, enabling you to leverage its functionalities within your workflows.

### Integrating Python Actor In To Workflow

The code below showcases how to integrate the `MPI` python iWrap actor (implementing the C++ code) into a larger workflow in python:

```{code-block} python
import imas
from imas import imasdef

from cp2ds_mpi.actor import cp2ds_mpi

# Reading of input data
db_entry_in = imas.DBEntry(backend_id=imasdef.MDSPLUS_BACKEND,
                           db_name="tutorial_db",
                           shot=1, run=1)
db_entry_in.open()
input_ids = db_entry_in.get('core_profiles')
db_entry_in.close()

# Creating output datafile
db_entry_out = imas.DBEntry(backend_id=imasdef.MDSPLUS_BACKEND,
                           db_name="tutorial_db",
                           shot=2, run=2)
db_entry_out.create()

# Actor creation
actor_mpi = cp2ds_mpi()

# MPI settings
runtime_settings = actor_mpi.get_runtime_settings()
runtime_settings.mpi.mpi_processes = 3

actor_mpi.initialize(runtime_settings=runtime_settings)

output_ids = actor_mpi(input_ids)

# Save IDS
db_entry_out.put(output_ids)

actor_mpi.finalize()
```

The example workflow configures the input and output databases as well as initializing the actors with proper MPI settings. Actor is run and output IDS is saved to the output database.
