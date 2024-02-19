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

# Introduction to MPI: Parallelizing Your Code with Runtime Flexibility

```{admonition} Note
:class: note

While this tutorial focuses specifically on iWrap, it does not delve into Message Passing Interface (MPI) concepts.
For comprehensive guidance on MPI, please refer to the following valuable resources:

- https://mpitutorial.com/
- https://researchcomputing.princeton.edu/education/external-online-resources/mpi
- https://hpc-tutorials.llnl.gov/mpi/
```

Message Passing Interface (`MPI`) is a standardized library that empowers you to harness the power of parallel computing by distributing your programs across multiple processors or cores. It provides fundamental communication functions like sending, receiving, and synchronizing data between processes, fostering collaboration and dramatically boosting performance for computationally intensive tasks.

Beyond its core functionality, `MPI` offers runtime settings that allow you to fine-tune and optimize your parallel applications. These settings, often adjustable through environment variables or command-line arguments, influence `MPI`'s internal behaviors, communication strategies, and resource allocation. Some of the possible settings are listed below, but they are just a glimpse into the extensive range of possibilities.

- Number of processes: Specify the desired number of processes to launch your application across eg. `mpirun -n 8 ./example_mpi_code`
- Process placement: Control how processes are mapped to available processors or cores for optimal performance eg. `mpirun --bind-to core --map-by core ./example_mpi_code`
- Debug mode: Enable comprehensive debugging information to pinpoint communication issues eg. `mpirun -debug ./example_mpi_code`
- Performance profiling: Gather detailed statistics on communication patterns and resource utilization for performance analysis eg. `mpirun -prof ./example_mpi_code`
