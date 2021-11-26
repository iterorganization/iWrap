#######################################################################################################################
iWrap user manual
#######################################################################################################################



Introduction
#######################################################################################################################

.. note::


    *Glossary*

    Scenario (aka workflow)
      A set of components (actors) constituting a directed graph to execute a computing algorithm

      Actors are dependent: a control and data is passed from actor to actor
      Usually the order of actors execution and the way how data are passed from actor to actor is managed by so called "workflow system". Such manager can be a simple script (codes) or more sophisticated "orchestrator" (e.g. Kepler)

    Actor
        A basic component of scenario / workflow
        An actor performs some actions (e.g. computations, visualisation, etc)
        Usually given actor consumes results provided by a previous actor in a scenario and produces data for a next actor in a scenario
        Actor API strictly depends on targeted workflow system: an orchestrator "fires" particular actions on actor
        An actor, using its internal mechanisms ('wrappers') calls 'native code' method(s), usually written in other language than an actor

    Native code
     A physics model, of standardised signature, provided by software developer

Motivations
#######################################################################################################################

Complex simulations often combines a number of physics codes, potentially provided by various actors and written in different programming languages. To make them working together, an additional layer, that 'orchestrates' execution of particular codes, and takes care on passing data between 'producers' and 'consumers' is needed. Sometimes the functionality of such layer is provided by dedicated software (aka 'workflow orchestrators', like Kepler https://kepler-project.org/), sometimes it can be handled by much simpler mechanism like Python scripts. Unfortunately all components ('actors') that constitute a computing scenario ('workflow') must be implemented in the same programming language, defining the same API.

Unfortunately, in most cases, scientific, simulation codes that performs computing intensive calculations (due to performance reasons) are written in C++ or Fortran, while 'workflow orchestrators' are implemented in (more popular nowadays) languages, like Java, Python, etc. Hence the need for a 'wrapper' that intermediates between native code language and language of the orchestrator. Such wrappers can be developed manually, however users may benefit from a tool that automatise this process - iWrap

iWrap
#######################################################################################################################


.. image:: /resources/attachments/iwrap_main_wnd.png

iWrap is a modular component generator, implemented in Python, used for creating IMAS actors from physics models. This mechanism allows to integrate physics codes written in one language (Fortran, CPP) within complex computing scenarios designed in other language (e.g. Python).

It's plug-in based modular design with clear separation of concerns allows to generate various types of actors and easily change data access paradigm (from dataset descriptor for AL to direct HDC data for instance)

**iWrap goals:**

iWrap creates a Python script (aka an actor) that:

* Calls a user code
* Provides error handling
* Calls debugger (if run in "debug" mode)
* Runs MPI code

Wrap generates a Fortran/CPP wrapper, which intermediates between Python script (workflow) and user code in terms of:
* Calling physical model methods – the native code provided by developer
* Converting arguments passed between the native code and computing scenario from/to the workflow programming language to/from the physics model programming language
* Managing the temporary IDS  (Interface Data Structures) storage used to pass data between workflow and physic model subroutines run in separate process as standalone executable.


**iWrap interfaces:**

For user conveniency iWrap provides two kinds of interfaces:

* User friendly graphical interface that allows non-experienced users to define an actor in intuitive way
* Command line interface foreseen for more advanced users that may want to e.g. automatise actor generation process using scripts



Preparation of the code and actor generation
#######################################################################################################################


.. toctree::
   :maxdepth: 1



   /resources/native_code.rst
   /resources/actor_generation.rst
   /resources/actor_usage.rst




