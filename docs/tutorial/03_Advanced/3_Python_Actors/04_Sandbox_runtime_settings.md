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

### Example Usage

Below is an example of how to configure and use sandbox settings in your actor:

```python
from my_actor_package.actor import MyActor
from my_actor_package.common.runtime_settings import SandboxMode, SandboxLifeTime

# Initialize and configure the actor
actor = MyActor(config)

# gets runtime settings
runtime_settings = actor.get_runtime_settings()

# configures sandbox settings
runtime_settings.sandbox.mode = SandboxMode.MANUAL
runtime_settings.sandbox.path = '/path/to/existing/sandbox/directory'
runtime_settings.sandbox.life_time = SandboxLifeTime.WORKFLOW_RUN

# updates runtime settings
actor.initialize(runtime_settings=runtime_settings)
```

## Detailed Description of Sandbox Settings

### `mode`

Specifies how the sandbox is managed. You can choose between manual and automatic modes.

#### `SandboxMode.MANUAL`

In manual mode, it is the developer's responsibility to maintain the sandbox. This includes creating the directory, cleaning it up, and any other necessary maintenance. The `path` attribute must be set to a valid directory.

```python
runtime_settings.sandbox.mode = SandboxMode.MANUAL
runtime_settings.sandbox.path = '/path/to/existing/sandbox/directory'
```

#### `SandboxMode.AUTOMATIC`

In automatic mode, the actor manages the sandbox, including its creation and cleanup. This is the default mode.

```python
runtime_settings.sandbox.mode = SandboxMode.AUTOMATIC
```

### `path`

Defines a valid path to an existing directory to be used as a sandbox in manual mode. In automatic mode, the directory is created by the actor.

```python
runtime_settings.sandbox.path = '/path/to/existing/sandbox/directory'
```

### `life_time`

Defines when the sandbox will be cleaned up and removed. You can choose between actor run, workflow run, and persistent.

#### `SandboxLifeTime.ACTOR_RUN`

The sandbox directory is cleaned before and after every main actor method execution.

```python
runtime_settings.sandbox.life_time = SandboxLifeTime.ACTOR_RUN
```

#### `SandboxLifeTime.WORKFLOW_RUN`

The sandbox directory is cleaned during the initialization stage of an actor and after other finalization actions. The sandbox should be available during the whole workflow run.

```python
runtime_settings.sandbox.life_time = SandboxLifeTime.WORKFLOW_RUN
```

#### `SandboxLifeTime.PERSISTENT`

The content of the sandbox directory is preserved and never cleaned up.

```python
runtime_settings.sandbox.life_time = SandboxLifeTime.PERSISTENT
```
