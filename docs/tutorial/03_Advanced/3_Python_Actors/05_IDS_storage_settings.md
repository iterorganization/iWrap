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

# IDS Storage Settings for Python Actors

```{admonition} Key takeaways
:class: tip

- IDS storage settings are crucial for managing temporary data storage during the execution of Python actors, ensuring efficient handling of IDS entries.
- The settings include parameters such as `db_name`, `backend`, and `persistent_backend`, which control the database name and the backends used for temporary and persistent storage.
- Proper configuration of IDS storage settings, including the selection of appropriate backends, enhances the performance and reliability of data management in different execution environments.
```

In this chapter, we will explore the IDS storage settings available for Python actors. These settings define the temporary storage used while passing IDS entries between the actor and the code. Understanding these settings allows you to manage data storage effectively during the execution of your actors.

## Overview of IDS Storage Settings

The IDS storage settings specify the configuration of the temporary storage used for handling IDS entries. These settings include parameters such as the database name, backend, and persistent backend, which control how and where data is stored during the actor's execution.

### Key IDS Storage Settings

The main IDS storage settings you can configure are:

- **db_name:** Name of the database to be used.
- **backend:** Backend to be used for temporary data storage.
- **persistent_backend:** Backend to be used when temporary data cannot be stored in memory, such as when running the actor in standalone mode.

### Default Values

- **db_name:** `'tmp'`
- **backend:** `imas.imasdef.MEMORY_BACKEND`
- **persistent_backend:** `imas.imasdef.MDSPLUS_BACKEND`

### Example Usage of IDS storage settings

Below is an example of how to configure and use IDS storage settings in your actor. Let's extend the example from the chapter before with new settings:

```python
import imas
from imas import imasdef

from cp2ds_mpi.actor import cp2ds_mpi
from cp2ds_mpi.common.runtime_settings import SandboxMode, SandboxLifeTime

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

actor = cp2ds_mpi()

# gets runtime settings
runtime_settings = actor.get_runtime_settings()

# MPI settings
runtime_settings.mpi.mpi_processes = 3

# Sandbox settings
runtime_settings.sandbox.mode = SandboxMode.MANUAL
runtime_settings.sandbox.path = '/pfs/work/g2bpogo/iwrap-sandbox/my_sandbox_dir'
runtime_settings.sandbox.life_time = SandboxLifeTime.PERSISTENT

# configures IDS storage settings
runtime_settings.ids_storage.persistent_backend = imas.imasdef.HDF5_BACKEND

# updates runtime settings
actor.initialize(runtime_settings=runtime_settings)

output_ids = actor(input_ids)

# Save IDS
db_entry_out.put(output_ids)

actor.finalize()
```

As a result, `my_sandbox_dir` saved `core_profiles` in temporary dir in `HDF5` format.

```
my_sandbox_dir
├── main.in
├── main.out
└── tmp
    └── 3
        ├── 0
        └── 1
            └── 1
                ├── core_profiles.h5
                └── master.h5
```

If you want a detailed description of IDS storage settings, visit [IDS storage settings documentation](../../../documentation/actor_usage.rst#IDS storage settings)