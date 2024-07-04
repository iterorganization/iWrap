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

# Quick Overview of MUSCLE3 & MUSCLE3 actors

```{admonition} Key takeaways
:class: tip

- MUSCLE3 enables the construction of multiscale simulations by integrating single-scale models into a unified framework, facilitating complex simulations across different scales.
- MUSCLE3 actors are persistent entities that communicate directly over networks or shared memory, ensuring efficient and continuous data exchange.
- The high-level configuration using yMMSL allows for straightforward definition and management of model coupling and execution, enhancing the modularity and flexibility of simulation workflows.
```

In this chapter, we will delve into the MUSCLE3 exploring how MUSCLE3 facilitates the building of multiscale simulations from single-scale models. We will also discuss the various aspects and functionalities of MUSCLE3 actors as well as how communication between actors is managed.

## Introduction to MUSCLE3

MUSCLE3 is a coupling library developed by the e-MUSC project of the University of Amsterdam and the Netherlands eScience Center. It is designed for building multiscale simulations by integrating single-scale models into a cohesive framework. To learn more, visit [MUSCLE3 official documentation](https://muscle3.readthedocs.io/en/latest/), [yMMSL specification](https://ymmsl-python.readthedocs.io/en/master/) and [iWrap MUSCLE3 plugin documentation](https://sharepoint.iter.org/departments/POP/CM/IMDesign/Code%20Documentation/iWrap-plugins/MUSCLE3/resources/code_wrapping.html)

## MUSCLE3 Actor

MUSCLE3 actors are persistent entities that interact over networks or shared memory. Here are some key characteristics of MUSCLE3 actors:

- **Not a Python Script**: A simulation model is a program, not a library.
- **Separate Actors for Each Domain**: Each process/domain is represented by a different actor.
- **Persistent Communication**: Actors communicate persistently over the network or shared memory.
- **Peer-to-Peer Communication**: Actors communicate directly with each other.
- **High-Level Configuration**: Model coupling is defined in a high-level configuration file (yMMSL).
- **Signal-Based Execution**: Execution depends on received signals.

## MUSCLE3 Actors: Functionality and Limitations

### Functionality

MUSCLE3 actor generators provide the following functionalities:

- **Communication with MUSCLE3 System**: Actors communicate with the MUSCLE3 system through standardized protocols.
- **Inter-Model Communication Management**: Managing communication between different models.
- **Checkpointing and Restarting**: Handling actor checkpointing and restarting to ensure continuity and fault tolerance.

### Limitations

- **Restricted to MUSCLE3 Micro Models**: The generated actors are currently limited to MUSCLE3 micro models.
- **Manual Coupling**: Generated models must be coupled manually using yMMSL.

## Example of a yMMSL Configuration

Here is an example of a yMMSL configuration file:

```yaml
model:
  name: example
  components:
    macro: macro
    micro: micro
  conduits:
    macro.core_profiles_out: micro.core_profiles_in
    micro.dist_sources_out: macro.dist_sources_in

settings:
  t_max: 2.0
  dt: 1.0

implementations:
  micro:
    executable: ${ACTORS_DIR}/m3_actor/bin/m3_actor.exe
  macro:
    executable: ${EXAMPLE_DIR}/macro.exe
```

## Communication Management in MUSCLE3

### Wrapper-Managed Communication

Communication between actors is managed solely by the wrapper. The actor sends and receives IDSes to/from other models.

### Ports and Arguments

Each port handles only one in/out argument (IDS). The number of ports and their names correspond to the arguments of the wrapped code.

### MPI Communication

For MPI-enabled actors, data are sent and received only by the root MPI process. The transferred IDS are serialized by Access Layer.
