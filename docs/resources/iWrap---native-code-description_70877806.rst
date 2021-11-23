
############################################################
Actor and native code YAML description
############################################################
.. contents::
.. sectnum::

Introduction
#######################################################################################################################

iWrap, to properly wrap the code, needs detailed informations about both: the wrapped code and an actor to be
generated. A formal description of the code provides information about the programming language used, arguments
passed to/from the code, type of these arguments, etc, etc, while an actor description tells iWrap how to name generated
actor, where to put it, etc. Such descriptions has to be provided in YAML format file, prepared manually, or
automatically with help of iWrap GUI.

.. note::
      iWrap GUI allows to generate an actor without the need for manual preparation of actor/code description.

**YAML file syntax**

The YAML file consists of two independent parts, with entry names corresponding to their roles:
*actor_description* and *code_description*. Only *code description* part is mandatory, and  *actor description* data
could be provided in a file or using iWrap commandline switches or interacting with GUI.

The structure of the file is following:

.. code-block:: YAML

    ---
    # actor description part - optional
    actor_description:
         # <see chapter below for details>

    # code description part - mandatory
    code_description:
         # <see chapter below for details>
    ...

.. warning::
      -  All YAML fields are *MANDATORY*, unless explicitly described as *OPTIONAL*
      -  An actor description part must begin with entry "actor_description:"
      -  A code description part must begin with entry "code_description:"

Actor description
#######################################################################################################################

Actor description syntax
=========================================================================================
-  actor_name:

   -  meaning: the arbitrary, user defined name of the actor. It determines e.g. : the name of class to be generated and directory where actor will be put
   -  value: string
   -  example: 'core2dist'

-  actor_type:

   -  meaning:
   -  values: 'python' (currently only python type has been implemented)
   -  example: 'python'

-  data_type:

   -  meaning: data type handled at the workflow level
   -  value: 'legacy' (currently only 'Legacy IDS' type has been implemented)
   -  example: 'legacy'

Example
=========================================================================================

.. code-block:: YAML

    ---
    actor_description:
        actor_name: core2dist
        actor_type: python
        data_type: legacy

    code_description:
        # mandatory part
    ...

Native code description
#######################################################################################################################

Description of the native code has to be provided as a YAML document. It consist of two parts. The first one contains
generic information common for all languages, The latter one contains information specific for a given language of the
native code (currently defined only for Fortran and CPP).

Common part
=========================================================================================

             Generic information common for all programming languages handled by iWrap:

            -    programming_language

               -  meaning:  language of physics code
               -  value: one of predefined values: 'Fortran', 'CPP'
               -  example: 'Fortran'

            -  *  code_name  *

               -  meaning:

                  -  name of user method / subroutine to be called,
                  -  must be \ **exactly the same** as name of called  method / subroutine
                  -  it is used also as an actor name and the name of
                     directory where actor is installed

               -  value: string
               -  example: 'my_subroutine'

            -  *  data_type  *

               -  meaning: data type handled by the physics code
               -  value: 'legacy' (currently only 'Legacy IDS' type has been implemented)
               -  example: 'legacy'

            -  *  arguments   * *- *\ list of arguments

               -  argument definition:

                  -  *name*:

                     -  meaning: user defined argument name
                     -  value: string
                     -  example: equilibrium00

                  -  *type*:

                     -  meaning: a type of an IDS argument
                     -  value: predefined name of one of the IDSes
                     -  example: 'equilibrium'

                  -  intent

                     -  meaning: determines if given argument is input
                        or output one
                     -  value: predefined - string "IN", "OUT"

            -  code_path:

               -  meaning: path to system library (C, C++) , script (Python), etc containing the physics code, including
                  method/subroutine to be run
               -  value: string, valid path to file
               -  example: 'any text'

            -  *  code_parameters  *\ ** ** - a structure containing
                 parameters   and schema   entry  :

               -    parameters   :

                  -  meaning: path to XML file containing user defined
                     parameters of the physics code
                  -  value: string, valid path to file
                  -  example: './code_parameters/parameters.xml'

               -    schema   :

                  -  meaning: path to XSD file contains schema of XML
                     parameters, to be able to validate them
                  -  value: string, valid path to file
                  -  example: './code_parameters/parameters.xsd'

            -  *  documentation   :*

               -  meaning: human readable description of native code
               -  value: string
               -  example: 'any text'

Language specific part - Fortran/C++
=========================================================================================

Syntax
------------------------------------------------------------
            -    compiler   :

               -  meaning: the name/vendor of the compiler (and not
                  compiler command!) used to compile native codes
               -  value: string, one of vendors of compilers, currently:
                  'Intel' or 'GCC'
               -  example: 'Intel'

            -    mpi_flavour

               -  meaning: MPI compiler flavour to be used
               -  values: string, one of:  MPICH, MPICH2, MVAPICH2,
                  OpenMPI, etc.
               -  example 'MPICH2'

            -    open_mp   :

               -  meaning: if user code should be compiled with OpenMP
                  flag
               -  values: boolean
               -  example 'true'

            -  *  system_libraries   :*

               -  meaning: a list of system libraries, managed
                  using *pkg-config*\  mechanism,  that has to be used
                  while native code linking

               -  value: a list of system libraries names, as they are
                  published by *pkg-config*

               -  example:

                  .. container:: table-wrap

                     +-----------------------------------------------------------------------+
                     | |   - fftw3f                                                          |
                     | |   - glib                                                            |
                     | |   - mkl                                                             |
                     +-----------------------------------------------------------------------+

            -    custom_libraries   :

               -  meaning: additional libraries, not managed
                  by *pkg-config*\  mechanism, necessary to link of the
                  physics code\ * *:

               -  value:  a list of paths to libraries

               -  example:

                  .. container:: table-wrap

                     +-----------------------------------+
                     | |   - ./lib/custom/libcustom1.a   |
                     | |   - ./lib/custom/libcustom2.a   |
                     +-----------------------------------+

Example - Fortran code description
------------------------------------------------------------

.. code-block:: YAML

    code_description:
        implementation:
            subroutines:
                init:   init_code
                main:   code_lifecycle
                finalize: clean_up
            programming_language: Fortran
            data_type: legacy
            code_path: ./native_code/libcode_lifecycle.a
            include_path: ./native_code/mod_code_lifecycle.mod
            code_parameters:
                parameters: ./input/input_physics.xml
                schema: ./input/input_physics.xsd
        arguments:
        -   name: equilibrium_in
            type: equilibrium
            intent: IN
        -   name: equilibrium_out
            type: equilibrium
            intent: OUT
        documentation: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
            eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
            veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
            consequat. '
        settings:
            compiler_cmd: gfortran
            mpi_compiler_cmd:
            open_mp_switch: false
            extra_libraries:
                pkg_config_defined:
                        - xmllib
                path_defined:

