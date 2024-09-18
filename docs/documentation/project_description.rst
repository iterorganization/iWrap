
.. _yaml_project_description_anchor:

############################################################
Actor and code YAML descriptions
############################################################

Introduction
#######################################################################################################################

iWrap, to properly wrap the code, needs detailed informations about both the code and the targeted actor to be
generated. A formal description of the code provides information about the programming language used, arguments
passed to/from the code's routines, type of these arguments, etc... The actor description tells iWrap how to name
the generated actor, where to install it, etc... Such descriptions have to be provided in a YAML file, prepared manually
or automatically with the help of iWrap's GUI.

.. note::
      iWrap GUI will guide the user through the description of the actor and code, without the need for manually writing the YAML file.

**YAML file syntax**

The YAML file consists of two independent parts, with entry names corresponding to their roles:
*actor_description* and *code_description*. Only *code_description* part is mandatory, and  *actor_description* data
could be provided in a file or using iWrap commandline switches or through the GUI.

The structure of the file is the following:

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
      -  The actor description part must begin with entry "actor_description:"
      -  The code description part must begin with entry "code_description:"

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

Code description
#######################################################################################################################

Description of the code has to be provided as a YAML document. It consist of two parts. The first one contains
generic information common for all languages, The latter one contains information specific for a given code
language.

Generic part
=========================================================================================

Generic information common for all programming languages handled by iWrap:

-   *implementation:*

    -   *programming_language:*

        -   meaning:  language used to implement the code's API
        -   value: one of predefined values: 'Fortran', 'CPP', 'Java'
        -   example: 'Fortran'

    -   *data_dictionary_compliant:*

        -   meaning: oldest known version of the Data Directory which is compatible with the code
        -   value: any string representing Data Directory version
        -   example: '3.37.0'

    -   *root_dir:*

        -   **optional** entry
        -   meaning:  the root directory for ALL relative paths placed in code description
        -   value: string, a relative path leading from YAML file location to actor project root dir
        -   example: '..'

    -   *subroutines:*

        -   *init*: a structure containing init function description. **optional**  :

            -   *name:*

                -  meaning: user defined function name
                -  value: string
                -  example: init_code

            -   *arguments*: a structure containing function arguments description.
                    For more information see *arguments* definition in *main:* function entry.

            -   *need_code_parameters:*

                -  meaning: flag, set to true, if function needs code parameters
                -  value: boolean
                -  example: true

        -   *main:* a structure containing main function description.  :

            -   *name:*

                -  meaning: user defined function name
                -  value: string
                -  example: code_step

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

            -   *need_code_parameters:*

                -  meaning: flag, set to true, if function needs code parameters
                -  value: boolean
                -  example: true

        -   *finalize:* a structure containing finalize function description.  :

            -   *name:*

                -  meaning: user defined function name
                -  value: string
                -  example: finalize

            -   *arguments*: a structure containing function arguments description.
                For more information see *arguments* definition in *main* function entry.

            -   *need_code_parameters:*

                -  meaning: flag, set to true, if function needs code parameters
                -  value: boolean
                -  example: true

        -    *get_state:*

             - **optional** entry
             -   meaning:

                 -  name of the method / subroutine to be called
                 -  must be **exactly the same** as name of called  method / subroutine
                 -  it is used to get the current state of the code

             -  value: string
             -  example: 'get_state'

        -    *set_state:*

             - **optional** entry
             -   meaning:

                 -  name of the method / subroutine to be called
                 -  must be **exactly the same** as name of called  method / subroutine
                 -  it is used to set a new current state for the code

             -  value: string
             -  example: 'set_state'

        -    *get_timestamp:*

             - **optional** entry
             -   meaning:

                 -  name of the method / subroutine to be called
                 -  must be **exactly the same** as name of called  method / subroutine
                 -  it is used to get a timestamp of the simulation performed by the code

             -  value: string
             -  example: 'get_timestamp'

    -   *data_type:*

        -   meaning: data type handled by the code's API
        -   value: 'legacy' (currently only 'Legacy IDS' type has been implemented)
        -   example: 'legacy'

    -  *code_path:*

       -  meaning: path to system library (C, C++, Fortran) , script (Python), jar (Java), etc., containing the code, including
          methods/subroutines to be called
       -  value: string, valid path to file
       -  example: '/path/to/code/lib/libcode.a'

    -  *include_path:*

       -  meaning: path to a header file (C, C++), module (Fortran), etc., containing the declaration of the code's API
       -  value: string, valid path to file
       -  example: '/path/to/code/include/code.h'

          .. warning::
            Remember that Fortran is case insensitive and modules (even if named by user using capital letters)
            generated by compiler are lowercase.
            Please check if the name of generated module file provided in YAML is correct!


    -   *code_parameters:* a structure containing parameters, schema and format entry. **optional**  :

        -   *parameters:*

            -  meaning: path to XML file containing default parameters of the code
            -  value: string, valid path to file
            -  example: './code_parameters/parameters.xml'

        -   *schema:*

            -  meaning: path to XSD file contains schema of XML parameters, enabling its validation
            -  value: string, valid path to file
            -  example: './code_parameters/parameters.xsd'

        -   *format:*

            -   **optional** entry
            -   meaning: format of the code parameters
            -   value: string, one of the supported formats: `legacy-xml` (default), `xml`, `json`, `namelist`
            -   example: 'xml'

                .. note::
                        Selecting `legacy-xml` format allows to keep backward compatibility with existing codes
                        that received code parameters packed in IMAS Access Layer structure: `ids_parameters_input`
                        (Fortran) or `IdsNs::codeparam_t` (C++).
                        If any other format is chosen, code parameters are passed as a string.


-   *documentation:*
    - **optional** entry
    -  meaning: human readable description of the actor
    -  value: string
    -  example: 'any text describing a the actor'

-   *settings:*  mandatory entry gathering all information specific for given language (see chapter below)


Language specific settings - Fortran/C++
=========================================================================================

Syntax
------------------------------------------------------------
-   *compiler_cmd:*

    -  meaning: the name of the compiler command used to compile the code and which will compile the wrapper
    -  value: string, compiler script name
    -  example: 'gfortran', 'ifort'

-   *mpi_compiler_cmd*:

    -  meaning: the name of the *MPI* compiler command used to compile the code and which will compile the wrapper
    -  value: string, compiler script name
    -  example: 'mpif90', 'ifort'
    -  Important! The existence (or absence) of this entry, determines if the code uses MPI or not

-   *compiler_flags:*

    -  meaning: a set of compiler flags used during compilation
    -  value: string
    -  example: '-std=f2008', '-qopenmp', '-ansi'

-   *extra_libraries:* -

    -  *pkg_config_defined:*

       -  meaning: a list of system libraries, managed using *pkg-config* mechanism, that has to be used
          while linking with the code's library

       -  value: a list of system libraries names, as they are published by *pkg-config*

       -  example:

          .. code-block:: YAML

                pkg_config_defined:
                     - fftw3f
                     - glib
                     - mkl

    -   *path_defined:*

        -  meaning: a list of additional libraries, not managed by *pkg-config* mechanism but necessary
           to link with the code's library

        -  value:  a list of paths to libraries

        -  example:

           .. code-block:: YAML

               path_defined:
                   - ./lib/custom/libcustom1.a
                   - ./lib/custom/libcustom2.a


Example - description of an actor wrapping a Fortran code
=========================================================================================

.. code-block:: YAML

    actor_description:
      actor_name: core2dist
      actor_type: python
      data_type: legacy
    code_description:
      documentation: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. '
      implementation:
        code_parameters:
          parameters: ./input/input_physics.xml
          schema: ./input/input_physics.xsd
        code_path: ./native_code/libcode_lifecycle.a
        data_dictionary_compliant: 3.39.0
        data_type: legacy
        include_path: ./native_code/mod_code_lifecycle.mod
        programming_language: Fortran
        subroutines:
          finalize:
            name: clean_up
          init:
            name: init_code
            need_code_parameters: true
          main:
            arguments:
            - intent: IN
              name: equilibrium_in
              type: equilibrium
            - intent: OUT
              name: equilibrium_out
              type: equilibrium
            name: code_lifecycle
            need_code_parameters: true
      settings:
        compiler_cmd: gfortran
        compiler_flags:
        extra_libraries:
          path_defined:
          - ./lib/custom/libcustom1.a
          - ./lib/custom/libcustom2.a
          pkg_config_defined:
          - xmllib
        mpi_compiler_cmd: mpif90