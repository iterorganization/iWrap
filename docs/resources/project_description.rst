.. _yaml_project_description_anchor:

############################################################
Actor and native code YAML description
############################################################

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

.. _yaml_actor_description_anchor:

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

.. _yaml_code_description_anchor:

Native code description
#######################################################################################################################

Description of the native code has to be provided as a YAML document. It consist of two parts. The first one contains
generic information common for all languages, The latter one contains information specific for a given native code
language (currently defined only for Fortran and C++).

Generic part
=========================================================================================

Generic information common for all programming languages handled by iWrap:

-   *implementation:*

    -   *programming_language:*

        -   meaning:  language of physics code
        -   value: one of predefined values: 'Fortran', 'CPP'
        -   example: 'Fortran'

    -   *data_dictionary_compliant:*

        -   meaning:  oldest known version of Data Directory compatible with actor
        -   value: any string representing Data Directory version
        -   example: '3.37.0'

    -   *root_dir:*

        -   **optional** entry
        -   meaning:  the root directory for ALL relative paths placed in code description
        -   value: string, a relative path leading from YAML file location to actor project root dir
        -   example: '..'

    -   *subroutines:*

        -   *init*:

            - **optional** entry
            -   meaning:

                -  name of user method / subroutine to be called,
                -  must be **exactly the same** as name of called method / subroutine
                -  it is used, usually, to set up the native code, however subroutine may contain any arbitrary actions
            -  value: string
            -  example: 'init_code'

        -   *main:*

            -   meaning:

                -  name of user method / subroutine to be called,
                -  must be \ **exactly the same** as name of called  method / subroutine

            -  value: string
            -  example: 'my_subroutine'

        -    *finalize:*

             - **optional** entry
             -   meaning:

                 -  name of user method / subroutine to be called
                 -  must be **exactly the same** as name of called  method / subroutine
                 -  it is used, usually, to clean up the native code, however subroutine may contain any arbitrary actions

             -  value: string
             -  example: 'clean_up'

        -    *get_state:*

             - **optional** entry
             -   meaning:

                 -  name of user method / subroutine to be called
                 -  must be **exactly the same** as name of called  method / subroutine
                 -  it is used, usually, to get current state of native code, however subroutine may contain any arbitrary actions

             -  value: string
             -  example: 'get_state'

        -    *set_state:*

             - **optional** entry
             -   meaning:

                 -  name of user method / subroutine to be called
                 -  must be **exactly the same** as name of called  method / subroutine
                 -  it is used, usually, to set current state of native code, however subroutine may contain any arbitrary actions

             -  value: string
             -  example: 'set_state'

        -    *get_timestamp:*

             - **optional** entry
             -   meaning:

                 -  name of user method / subroutine to be called
                 -  must be **exactly the same** as name of called  method / subroutine
                 -  it is used, usually, to get timestamp of native code, however subroutine may contain any arbitrary actions

             -  value: string
             -  example: 'get_timestamp'

    -   *data_type:*

        -   meaning: data type handled by the physics code
        -   value: 'legacy' (currently only 'Legacy IDS' type has been implemented)
        -   example: 'legacy'

    -  *code_path:*

       -  meaning: path to system library (C, C++, Fortran) , script (Python), etc., containing the physics code, including
          methods/subroutines to be run
       -  value: string, valid path to file
       -  example: '/path/to/code/lib/libcode.a'

    -  *include_path:*

       -  meaning: path to a header file (C, C++), module (Fortran), etc., containing the declaration of physics code
          methods/subroutines to be run
       -  value: string, valid path to file
       -  example: '/path/to/code/include/code.h'

          .. warning::
            Remember that Fortran is case insensitive and modules (even if named by user using capital letters)
            generated by compiler are lowercase.
            Please check if the name of generated module file provided in YAML is correct!


    -   *code_parameters:* a structure containing parameters and schema entry. **optional**  :

        -   *parameters:*

            -  meaning: path to XML file containing user defined parameters of the physics model
            -  value: string, valid path to file
            -  example: './code_parameters/parameters.xml'

        -   *schema:*

            -  meaning: path to XSD file contains schema of XML parameters, enabling its validation
            -  value: string, valid path to file
            -  example: './code_parameters/parameters.xsd'



-   *arguments:* list of arguments. Argument definition:

    -   *name:*

        -  meaning: user defined argument name
        -  value: string
        -  example: equilibrium00

    -   *type:*

        -  meaning: a type of an IDS argument
        -  value: predefined name of one of the IDSes
        -  example: 'equilibrium'

    -   *intent:*

        -  meaning: determines if given argument is input or output one
        -  value: predefined - string "IN", "OUT"
        -  example: 'IN'

-   *documentation:*
    - **optional** entry
    -  meaning: human readable description of native code
    -  value: string
    -  example: 'any text describing a physics model'

-   *settings:*  mandatory entry gathering all information specific for given language (see chapter below)


Language specific settings - Fortran/C++
=========================================================================================

Syntax
------------------------------------------------------------
-   *compiler_cmd:*

    -  meaning: the name/vendor of the compiler command used to compile native codes
    -  value: string, compiler script name
    -  example: 'gfortran', 'ifort'

-   *mpi_compiler_cmd:

    -  meaning: the name/vendor of the *MPI* compiler command used to compile native codes.
    -  value: string, compiler script name
    -  example: 'mpif90', 'ifort'
    -  Important! The existence (or absence) of this entry, determines if native codes use MPI or not

-   *open_mp_switch:*

    -  meaning: a compiler switch to be used if native code use OpenMP.
    -  value: string
    -  example: '-fopenmp', '-qopenmp'

-   *extra_libraries:* -

    -  *pkg_config_defined:*

       -  meaning: a list of system libraries, managed using *pkg-config* mechanism, that has to be used
          while native code linking

       -  value: a list of system libraries names, as they are published by *pkg-config*

       -  example:

          .. code-block:: YAML

                pkg_config_defined:
                     - fftw3f
                     - glib
                     - mkl

    -   *path_defined:*

        -  meaning: a list of additional libraries, not managed by *pkg-config* mechanism but necessary
           to link the provided physics code

        -  value:  a list of paths to libraries

        -  example:

           .. code-block:: YAML

               path_defined:
                   - ./lib/custom/libcustom1.a
                   - ./lib/custom/libcustom2.a


Example - description of an actor wrapping Fortran code x
=========================================================================================

.. code-block:: YAML

    ---
    actor_description:
        actor_name: core2dist
        actor_type: python
        data_type: legacy

    code_description:
        implementation:
            subroutines:
                init:   init_code
                main:   code_lifecycle
                finalize: clean_up
            programming_language: Fortran
            data_directory_compliant: 3.37.0
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
            mpi_compiler_cmd: mpif90
            open_mp_switch: -qopenmp
            extra_libraries:
                pkg_config_defined:
                  - xmllib
                path_defined:
                  - ./lib/custom/libcustom1.a
                  - ./lib/custom/libcustom2.a
    ...
