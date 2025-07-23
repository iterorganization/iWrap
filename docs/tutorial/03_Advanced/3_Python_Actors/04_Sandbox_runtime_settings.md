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

# Sandbox Runtime Settings for Python Actors

```{admonition} Key takeaways
:class: tip

- Sandbox settings define the execution environment for Python actors, specifying how and where the actor's tasks will be executed.
- You can manage the sandbox either manually or automatically, with various options for the lifetime of the sandbox directory, such as actor run, workflow run, or persistent.
- Proper configuration of sandbox settings ensures that the execution environment is maintained according to the requirements of the workflow, improving the management and isolation of actor executions.
```

In this chapter, we will explore the sandbox runtime settings available for Python actors. A sandbox is a directory where the actor will be run. Understanding these settings allows you to manage the execution environment for your actors effectively.

## Overview of Sandbox Settings

The sandbox settings define the directory in which the actor will execute its tasks. Before the execution of user codes wrapped by an iWrap generated actor, the current directory is changed to the sandbox directory. After the actor finishes, the current directory is switched back to its previous value. The sandbox directory can either be automatically created by the actor or specified by the user.

### Key Sandbox Settings

The main sandbox settings you can configure are:

- **mode:** Defines how the sandbox is managed. One of the predefined values of the `SandboxMode` class.
- **path:** A valid path to an existing directory. This is used in 'manual' mode as a sandbox.
- **life_time:** Defines when the sandbox will be cleaned up and removed. One of the predefined values of the `SandboxLifeTime` class.

### SandboxMode Values

- **SandboxMode.MANUAL:** Full manual mode. It is the developer's responsibility to maintain the sandbox (i.e., create it, clean it up, etc.). Requires the `path` attribute to be set.
- **SandboxMode.AUTOMATIC:** The iWrap generated actor manages sandbox creation, cleanup, etc.

### SandboxLifeTime Values

- **SandboxLifeTime.ACTOR_RUN:** The content of the sandbox directory is cleaned before and after every main actor method execution.
- **SandboxLifeTime.WORKFLOW_RUN:** The content of the sandbox directory is cleaned during the initialization stage of an actor and after other finalization actions of the actor. The sandbox should be available during the whole workflow run.
- **SandboxLifeTime.PERSISTENT:** The content of the sandbox directory is preserved and never cleaned up.

### Example Usage of Sandbox using `MPI` Python Actor

In this example we will use the `MPI` code example from the [example MPI code chapter](../2_Advanced_Native_Code_Functionality/02_Example_MPI_code.md) and `YAML` code description from [python actor generation chapter](01_Python_actor_generation.md) to showcase the use of the sandbox settings.

Below is an example of how to configure and use manual sandbox mode with persistent life time in your workflow:

```python
import imas

from cp2ds_mpi.actor import cp2ds_mpi
from cp2ds_mpi.common.runtime_settings import SandboxMode, SandboxLifeTime

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

actor = cp2ds_mpi()

# gets runtime settings
runtime_settings = actor.get_runtime_settings()

# MPI settings
runtime_settings.mpi.mpi_processes = 3

# Sandbox settings
runtime_settings.sandbox.mode = SandboxMode.MANUAL
runtime_settings.sandbox.path = '/pfs/work/g2bpogo/iwrap-sandbox/my_sandbox_dir'
runtime_settings.sandbox.life_time = SandboxLifeTime.PERSISTENT

# updates runtime settings
actor.initialize(runtime_settings=runtime_settings)

output_ids = actor(input_ids)

# Save IDS
db_entry_out.put(output_ids)

actor.finalize()
```

Trying to run this workflow right away will result in an error

```sh
ValueError: Actor cp2ds_mpi: Sandbox path points to non existing directory (my_sandbox_dir)
```

This is because when running manual mode, you are responsible for managing the directory. When you create the directory, the directory will be filled with the outputs of the workflow and will persist throughout workflow runs.

If you want a detailed description of sandbox settings, visit [sandbox settings documentation](../../../documentation/actor_usage.rst#Sandbox settings)
