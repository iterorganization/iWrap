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

# Run Modes for Python Actors

```{admonition} Key takeaways
:class: tip

- Understanding and configuring the appropriate run mode for Python actors is crucial for optimizing their performance and resource utilization.
- Normal mode is ideal for development and debugging, as it runs the actor's code within the same process and environment as the workflow script.
- Standalone and Batch modes provide separate execution environments, with Batch mode being particularly useful for processing large volumes of data in a batch processing context.
```

In this chapter, we will explore the different run modes available for Python actors. Understanding these modes is essential for efficiently utilizing actors in various contexts. We will cover the following run modes:

- Normal Mode
- Standalone Mode

## Run Mode

The run mode is defined by setting one of the predefined `RunMode` enumeration class values.

```{code-block} python
from <actor name>.common.runtime_settings import RunMode

# gets runtime settings
runtime_settings = actor_object.get_runtime_settings()

#configures runtime settings
runtime_settings.run_mode = RunMode.NORMAL

# updates runtime_settings
actor_object.initialize(runtime_settings=runtime_settings)
```

The available run modes are:

### Normal Mode

Normal mode (`RunMode.NORMAL`) is the default execution context for Python actors. In this mode, the actor’s code is loaded as a library, and its routines are called directly from Python within the same process and environment as the workflow script. This mode is suitable for most actors, especially during development and debugging.

### Standalone Mode

Standalone mode (`RunMode.STANDALONE`) runs the actor’s code as an executable in a separate process, providing its own environment and often greater system resources. This mode is automatically set for MPI applications and can also be used for memory-intensive code.
