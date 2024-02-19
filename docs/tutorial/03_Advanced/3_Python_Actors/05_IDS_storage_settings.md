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

### Example Usage

Below is an example of how to configure and use IDS storage settings in your actor:

```python
import imas
from my_actor_package.actor import MyActor

# Initialize and configure the actor
actor = MyActor(config)

# gets runtime settings
runtime_settings = actor.get_runtime_settings()

# configures IDS storage settings
runtime_settings.ids_storage.backend = imas.imasdef.MDSPLUS_BACKEND
runtime_settings.ids_storage.persistent_backend = imas.imasdef.HDF5_BACKEND

# updates runtime settings
actor.initialize(runtime_settings=runtime_settings)
```

## Detailed Description of IDS Storage Settings

### `db_name`

Specifies the name of the database to be used for temporary storage. This setting allows you to define a custom database name if needed.

```python
runtime_settings.ids_storage.db_name = 'custom_db_name'
```

### `backend`

Defines the backend to be used for temporary data storage. The default backend is `imas.imasdef.MEMORY_BACKEND`, which stores data in memory.

```python
runtime_settings.ids_storage.backend = imas.imasdef.MEMORY_BACKEND
```

### `persistent_backend`

Specifies the backend to be used when temporary data cannot be stored in memory. This is important for scenarios such as running the actor in standalone mode, where the code is executed in a separate process and does not share memory with the workflow. The default persistent backend is `imas.imasdef.MDSPLUS_BACKEND`.

```python
runtime_settings.ids_storage.persistent_backend = imas.imasdef.MDSPLUS_BACKEND
```
