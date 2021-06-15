# iWrap
_**Disclaimer**_  
_iWrap is still under development, so all procedures described below are temporary, prepared for demonstration purposes only!_
## Description
iWrap is a modular component generator, implemented in Python, used for creating IMAS actors from physics models. This mechanism allows to integrate physics codes written in one language (Fortran, CPP) within complex computing scenarios designed in other language (e.g. Python).

It's plug-in based modular design with clear separation of concerns allows to generate various types of actors and easily change data access paradigm (from dataset descriptor for AL to direct HDC data for instance)

For user conveniency it provides two kinds of interfaces: 
* user friendly graphical interface that allows non-experienced users to define an actor in intuitive way 
* command line interface foreseen for more advanced users that may want to e.g. automatise actor generation process using scripts.

## Installation
    git clone ssh://git@git.iter.org/imex/iwrap.git
    cd iwrap
    git checkout develop

## Environment configuration
To configure an environment, please go to `iwrap` main directory and execute from commandline: 
> source set-iter.sh (on ITER IO cluster)  
> OR  
> source set-gw.sh (on EF Gateway)

The scripts provide very simple operations. It:
* purges modules
* loads `IMAS`
* sets `PATH` and `PYTHONPATH`

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
             [--list-actor-types] [--list-actor-details [ACTOR_TYPE]] [-v]

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
  -f FILE, --file FILE  a path to code description *.yaml file

Additional information:
  --list-actor-types    lists registered actor types that can be generated
  --list-actor-details [ACTOR_TYPE]
                        lists details of given actor type generator
  -v, --version         show program's version number and exit

For more information, visit <https://confluence.iter.org/display/IMP/IMAS+component+generator>.
```
## Actor generation
>iwrap -a <actor_name> -f <path/to/code_description.yaml>

... will generate an actor based on code description stored in yaml, where ...

>iwrap-gui -a <actor_name> -f <path/to/code_description.yaml>

... will launch iWrap GUI filled in with information coming from code description 

