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

# iWrap's MUSCLE3 Checkpointing Example

```{admonition} Key takeaways
:class: tip

- Incorporate `get_state` and `set_state` methods in the iWrap YAML description to enable checkpointing in MUSCLE3 simulations, allowing the simulation state to be saved and restored.
- Define checkpoints in the yMMSL file to specify when MUSCLE3 should save snapshots, using `simulation_time` and `wallclock_time` directives.
- Execute the simulation with `muscle_manager` and resume it from checkpoints, ensuring fault tolerance and continuity of long-running simulations.
```

In this chapter, we will delve into automatic checkpointing done by `MUSCLE3`, utilizing iWrap `get_state` and `set_state` methods. Checkpointing allows you to save the state of a simulation at certain intervals, enabling the recovery and continuation of the simulation from those points if a failure occurs. This example will extend the previous work by incorporating MPI for parallel computations.

## Reusing the Macro & Micro Code

We will reuse [MUSCLE3 macro & micro code from the previous chapter](04_muscle3_MPI.md) for this example.

## iWrap actor description

To enable checkpointing in `MUSCLE3`, we need to provide `get_state` and `set_state` methods in iWrap yaml description.

```{code-block} yaml
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

These methods will be used internally by `MUSCLE3` framework to save snapshots of the simulation.

## Writing the yMMSL File

Reusing the `yMMSL` example [from the previous example](04_muscle3_MPI.md#Writing the yMMSL File), we will add `checkpoint` directive to it. This description will tell `MUSCLE3` when to save snapshots. Possible entries are as follow:

```{code-block} yaml
checkpoints:
  at_end: true
  simulation_time:
  - every: 10
    start: 0
    stop: 100
  - every: 20
    start: 100
  wallclock_time:
  - every: 3600
  - at:
    - 300
    - 600
    - 1800
```

Checkpoints defined in `simulation_time` are based on time inside your simulation, while `wallclock_time` are based on real time elapsed. In this example, we will add a `wallclock_time` checkpoints every 0,5 seconds and one snapshot at the end.

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

checkpoints:
  at_end: true
  wallclock_time:
    - every: 0.5
```

## Running simulation

To run the simulation, execute the following command:

```{code-block} sh
muscle_manager --start-all --log-level DEBUG mpi-cpp.ymmsl
```

This will produce the following output

```
run_helloworld_20240724_132706
├── configuration.ymmsl
├── instances
│   ├── macro
│   │   ├── snapshots
│   │   │   └── macro_1.pack
│   │   ├── stderr.txt
│   │   ├── stdout.txt
│   │   └── workdir
│   └── micro
│       ├── snapshots
│       │   ├── micro_1.pack
│       │   └── micro_2.pack
│       ├── stderr.txt
│       ├── stdout.txt
│       └── workdir
├── muscle3_manager.log
├── performance.sqlite
├── qcgpj
└── snapshots
    └── snapshot_20240724_132708.ymmsl
```

As you can see, running the simulation will produce the snapshot folder with a snapshot that looks like this

``` {code-block} yaml
ymmsl_version: v0.1
description: |
  Workflow snapshot for helloworld taken on 2024-07-24 13:27:08.
  Snapshot triggers:
  - at_end (macro, micro)

  Instance  t           Wallclock time  Type
  --------------------------------------------------
  macro     nan         2.13698         Final
  micro     -1          2.13937         Final
resume:
  macro: /gss_efgw_work/work/g2bpogo/iwrap-muscle/run_helloworld_20240724_132706/instances/macro/snapshots/macro_1.pack
  micro: /gss_efgw_work/work/g2bpogo/iwrap-muscle/run_helloworld_20240724_132706/instances/micro/snapshots/micro_2.pack
```

You can resume the simulation by using

```{code-block} sh
muscle_manager --log-level DEBUG mpi-cpp.ymmsl snapshot_20240724_132708.ymmsl
```

To learn more, please visit [MUSCLE3 documentation on checkpointing](https://muscle3.readthedocs.io/en/latest/checkpointing.html)