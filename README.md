# iWrap
_**Disclaimer**_  
_iWrap is still under development, so its API and features may change!_

iWrap is a modular component generator, implemented in Python, used for creating IMAS actors from physics models. This mechanism allows to integrate physics codes written in one language (Fortran, CPP) within complex computing scenarios designed in other language (e.g. Python).

It's plug-in based modular design with clear separation of concerns allows to generate various types of actors and easily change data access paradigm (from dataset descriptor for AL to direct HDC data for instance)

For user conveniency it provides two kinds of interfaces: 
* user friendly graphical interface that allows non-experienced users to define an actor in intuitive way 
* command line interface foreseen for more advanced users that may want to e.g. automatise actor generation process using scripts.

## Tutorials

To get to know with iWrap please read `tutorials/README.md` for generating interactive and step-by-step tutorial in Jupyter Notebook. 

We have also prepared HTML and Docker version, so you can choose the one that suits you best.

# Configuration of working environment

## Downloading software
    git clone ssh://git@git.iter.org/imex/iwrap.git
    cd iwrap
    git checkout <branch>

## Environment configuration
To configure an environment, please go to `iwrap` main directory and execute from commandline: 
> source set-iter.sh (on ITER IO cluster)  
> OR  
> source set-gw.sh (on EF Gateway)

The scripts provide very simple operations. It:
* purges modules
* loads `IMAS`
* sets `PATH` and `PYTHONPATH`
* sets compiler flags like `CXX`, `FC`, `MPICXX`, `MPIFC`

## Launching iWrap
To launch _iWrap_, just execute on commandline:
* to run commandline version
    > iwrap
* to  run iWrap user interface
    > iwrap-gui
  
Both scripts share the same set arguments and switches
```This is a code block.
shell>iwrap -h
usage: iwrap [-h] [-a ACTOR_NAME] [-t ACTOR_TYPE] [-d DATA_TYPE] [-f FILE]
             [-i INSTALL_DIR] [--list-actor-types]
             [--list-actor-details [ACTOR_TYPE]] [-v]

iWrap - a modular component generator, used for creating IMAS actors from
physics models.

optional arguments:
  -h, --help            show this help message and exit

Actor generation:
  -a ACTOR_NAME, --actor-name ACTOR_NAME
                        user defined name of the actor
  -t ACTOR_TYPE, --actor-type ACTOR_TYPE
                        type of an actor to be generated
  -d DATA_TYPE, --data-type DATA_TYPE
                        type of data to be used by the actor
  -f FILE, --file FILE  a path to code/actor description *.yaml file
  -i INSTALL_DIR, --install-dir INSTALL_DIR
                        actor installation directory

Additional information:
  --list-actor-types    lists registered actor types that can be generated
  --list-actor-details [ACTOR_TYPE]
                        lists details of given actor type generator
  -v, --version         show program's version number and exit


For more information, visit <https://confluence.iter.org/display/IMP/IMAS+component+generator>.
```
## Actor generation
`shell> iwrap -a <actor_name> -f <path/to/code_description.yaml>`

... will generate an actor based on code description stored in yaml, where ...

`shell> iwrap-gui -a <actor_name> -f <path/to/code_description.yaml>`

... will launch iWrap GUI filled in with information coming from code description 

# Tutorials

iWrap provides its tutorials in two formats::
  - interactive Jupyter Notebooks `.ipynb`
  - static nested HTML pages in a book format `.html`

Start your tutorial journey by heading to the `tutorials` directory and opening the `README.md` file found there (it will explain how to set up and run tutorials in both formats and under different environments).


# Manuals
To launch a browser with documentation, simply run:

`shell> iwrap-doc`

# Examples
Examples placed in directory `iwrap/examples` can be an excellent source of knowledge
related to _code description_ syntax, actors API and the way actors are called from workflow.

## Introduction

A content of a directory is similar for all examples and consist of:
 
- `native_code` directory - containing a physics model to be wrapped 
- `YAML` file - providing all information essential for iWrap to generate an actor from the native code
- `Makefile` that simplifies all the steps requires from building a native code to running an example
-  Python script - containing simple scenario (aka workflow) that allow to run given example

## Prerequisites

Following software must be available to run the examples:

- IMAS built with Access Layer of version 4.11 (or later)
- XMLLib library - necessary to build and run actors that wraps code using XML parameters - should be available via pkg-config mechanism
- compiler flags: `CXX`, `FC`, `MPICXX`, `MPIFC` must be set

To simplify setting up a working environment, configuration scripts are available for two of the platforms
commonly utilized by IMAS users:
- `set-iter.sh` for the ITER Organisation computing cluster (SDCC)
- `set-gw.(ba)sh` for the EUROfusion Gateway

All scripts need to be **sourced**

## Building and running examples 

Following steps need to be performed to build and run an example:

1. Jump to example directory:
    ```Shell
    cd <example_dir>
    ```

2. Build the native code:
     ```Shell
    make native
    ```

3. Generate a Python actor:
    ```Shell
    make actor
    ```
   
4. Run a simple scenario:
    ```Shell
    make wf-run
    ```
