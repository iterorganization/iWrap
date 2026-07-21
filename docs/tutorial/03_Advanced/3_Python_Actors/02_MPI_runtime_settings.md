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

# Optimizing MPI Workflows: Understanding and Configuring MPI Runtime Settings

```{admonition} Key takeaways
:class: tip

- Properly configuring MPI runtime settings, such as the number of processes and MPI runners, is crucial for optimizing parallel processing tasks and enhancing the performance of MPI workflows.
- The Python workflow example demonstrates how to initialize, execute, and finalize MPI-based actors, highlighting the importance of setting appropriate runtime parameters for efficient execution.
- Custom MPI options can be defined to tailor the MPI environment for specific needs, including debugging and performance tuning, thereby improving the overall computational efficiency.
```

Parallel processing with MPI (Message Passing Interface) is a cornerstone of high-performance computing, enabling complex simulations and data processing tasks to be executed efficiently across multiple processors. This chapter delves into the essential aspects of configuring MPI runtime settings, offering a detailed guide to setting up and optimizing your MPI environment. By understanding and properly configuring parameters such as the number of processes, MPI runners, and custom options, you can significantly enhance the performance and debugging capabilities of your MPI workflows.

## Python Workflow Example using `MPI` Python Actor

Example from previous chapter will be used to present a workflow that uses `MPI` code.

```{code-block} python
import imas

from cp2ds_mpi.actor import cp2ds_mpi

# Reading of input data
db_entry_in = imas.DBEntry(backend_id=imas.ids_defs.MDSPLUS_BACKEND,
                           db_name="tutorial_db",
                           shot=1, run=1)
db_entry_in.open()
input_ids = db_entry_in.get('core_profiles')
db_entry_in.close()

# Creating output datafile
db_entry_out = imas.DBEntry(backend_id=imas.ids_defs.MDSPLUS_BACKEND,
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

As it is visible from the example above, it is possible to set `MPI` runtime settings like number of processes to run your program on.

### Introduction to MPI Runtime Settings

Runtime settings in MPI (Message Passing Interface) are crucial for configuring and optimizing parallel processing tasks. They define how the MPI environment should operate, including the number of processes, the MPI runner, and additional options for performance tuning and debugging.

In the context of MPI workflows, especially those involving complex simulations or data processing tasks, properly setting up runtime parameters can significantly impact the efficiency and effectiveness of the computation.

### Core Runtime Settings in MPI

#### `mpi_processes`

Description: Specifies the number of MPI processes to launch.
Usage: Defines how many parallel tasks the MPI program should run.
Example: Setting `mpi_processes = 4` will launch four parallel processes.

#### `mpi_runner`

Description: User-defined MPI runner, typically a command like `mpirun` or `mpiexec`.
Usage: Determines the tool used to initiate the MPI environment.
Example: Setting `mpi_runner = 'mpirun'` uses the `mpirun` command to start processes.

#### `mpi_options`

Description: Custom options for the MPI runner, useful for debugging or performance tuning.
Usage: Adds specific flags or options to the MPI runner command.
Example: Setting `mpi_options = '--debug'` enables debugging mode.

### Configuring Runtime Settings

#### Setting MPI Processes

To configure the number of MPI processes, adjust the mpi_processes parameter in your runtime settings. This is critical for balancing the load across available processors.

```python
runtime_settings.mpi.mpi_processes = 4
```

#### Choosing and Configuring MPI Runner

Select an appropriate MPI runner based on your system’s configuration and requirements. Common runners include `mpirun` and `mpiexec`.

```python
runtime_settings.mpi.mpi_runner = 'mpirun'
```

### Defining MPI Options

MPI options can be customized to enhance the MPI environment’s behavior. This includes debugging, specifying hosts, and more.

```python
runtime_settings.mpi.mpi_options = '--debug'
```
