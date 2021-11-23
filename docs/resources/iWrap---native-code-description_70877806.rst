:orphan:
======================================================
Scientific Worfklows : iWrap - native code description
======================================================

.. container::
   :name: page

   .. container:: aui-page-panel
      :name: main

      .. container::
         :name: main-header

         .. container::
            :name: breadcrumb-section

            #. `Scientific Worfklows <index.html>`__
            #. `Wrapping user codes into actors -
               iWrap <Wrapping-user-codes-into-actors---iWrap_70877391.html>`__

         .. rubric:: Scientific Worfklows : iWrap - native code
            description
            :name: title-heading
            :class: pagetitle

      .. container:: view
         :name: content

         .. container:: page-metadata

            Created by Bartosz Palak on 06 wrz 2021

         .. container:: wiki-content group
            :name: main-content

            .. rubric:: 1.Introduction 
               :name: iWrapnativecodedescription-Introduction
               :class: auto-cursor-target

            Description of the native code has to be provided as a YAML
            document. It consist of two parts. The first one contains
            generic information common for all languages, The latter one
            contains information specific for a given language of the
            native code (currently defined only for Fortran and CPP). 

            | 

            Please note - all YAML fields are MANDATORY, unless
            explicitly described as OPTIONAL

            .. rubric:: 2.Common part
               :name: iWrapnativecodedescription-Commonpart
               :class: auto-cursor-target

            -  *programming_language*

               -  meaning:  language of physics code
               -  value: one of predefined values: 'Fortran', 'CPP'
               -  example: 'Fortran'

            -  *code_name*

               -  meaning:

                  -  name of user method / subroutine to be called,
                  -  must be \ **exactly the same** as name of called
                     method / subroutine
                  -  used also as an actor name

               -  value: string
               -  example: 'my_subroutine' 

            -  *data_type*

               -  meaning: data type handled by the physics code
               -  value: one of predefined values: 'Legacy IDS' 'HDC
                  IDS'
               -  example: 'Legacy IDS'

            -  *arguments - *\ list of arguments

               -  argument definition: 

                  -  *name*:

                     -  meaning: user defined argument name
                     -  value: string
                     -  example: equilibrium00   

                  -  *type*:

                     -  meaning: a type of an IDS argument
                     -  value: 

                        -  for data type 'IDS': predefined name of one
                           of the IDSes
                        -  for data type 'HDC' : TBD

                     -  example: 'equilibrium' 

                  -  intent

                     -  meaning: determines if given argument is input
                        or output one
                     -  value: predefined - string "IN", "OUT"

            -  *code_path:* 

               -  meaning: path to system library (C, CPP) , script
                  (Python), etc containing the physics code, including
                  method/subroutine to be run
               -  value: string, valid path to file
               -  example: 'any text'  

            -  code_parameters:

               -  parameters:

                  -  meaning: path to XML file containing user defined
                     parameters of the physics code
                  -  value: string, valid path to file
                  -  example: './code_parameters/parameters.xml'

               -  schema:

                  -  meaning: path to XSD file contains schema of XML
                     parameters, to be able to validate them
                  -  value: string, valid path to file
                  -  example: './code_parameters/parameters.xsd'

            -  *documentation:*

               -  meaning: human readable description of native code
               -  value: string
               -  example: 'any text'

            .. rubric:: 3.Language specific part
               :name: iWrapnativecodedescription-Languagespecificpart

            .. rubric:: 3.1.Fortran/CPP
               :name: iWrapnativecodedescription-Fortran/CPP

            -  *compiler*:

               -  meaning: the name/vendor of the compiler (and not
                  compiler command!) used to compile native codes
               -  value: string, one of vendors of compilers, currently:
                  'Intel' or 'GCC'
               -  example: 'Intel'

            -  *mpi\_*\ flavour:

               -  meaning: MPI compiler flavour to be
                  used \ `|image1|\ IMAS-2056 <https://jira.iter.org/browse/IMAS-2056>`__\  - Allow
                  users to select MPI implementation as well as compiler
                  / linker ON HOLD
               -  values: string, one of:  MPICH, MPICH2, MVAPICH2,
                  OpenMPI, etc.
               -  example 'MPICH2'

            -  *open_mp*:

               -  meaning: if user code should be compiled with OpenMP
                  flag
               -  values: boolean
               -  example 'true'

            -  *system_libraries:*

               -  meaning: a list of system libraries, managed
                  using *pkg-config*\  mechanism,  that has to be used
                  while native code linking

               -  value: a list of system libraries names, as they are
                  published by *pkg-config* |(ostrzeżenie)|

               -  example: 

                  .. container:: table-wrap

                     +-----------------------------------------------------------------------+
                     | |   - fftw3f                                                          |
                     | |   - glib                                                            |
                     | |   - mkl                                                             |
                     +-----------------------------------------------------------------------+

            -  *custom_libraries*:

               -  meaning: additional libraries, not managed
                  by *pkg-config*\  mechanism, necessary to link of the
                  physics code\ * *:

               -  value:  a list of paths to libraries 

               -  example: 

                  .. container:: table-wrap

                     +-----------------------------------------------------------------------+
                     | |   - ./lib/custom/libcustom1.a                                       |
                     | |   - ./lib/custom/libcustom2.a                                       |
                     +-----------------------------------------------------------------------+

            .. rubric:: 4.Examples
               :name: iWrapnativecodedescription-Examples
               :class: auto-cursor-target

            .. rubric:: 4.1.Fortran code description
               :name: iWrapnativecodedescription-Fortrancodedescription

            .. container:: table-wrap

               +----------------------------------------------------------------------+
               | |   ---                                                              |
               | |   programming_language:  \  \   Fortran                            |
               | |   code_name:  \  \   demo_code                                     |
               | |   data_type:  \  \   LEGACY_IDS                                    |
               | |   arguments:                                                       |
               | |   -   name  \   :  \  \   equilibrium00                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   IN                                     |
               | |   -   name  \   :  \  \   equilibrium01                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   IN                                     |
               | |   -   name  \   :  \  \   equilibrium10                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   OUT                                    |
               | |   -   name  \   :  \  \   equilibrium11                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   OUT                                    |
               | |   code_path:  \  \   ./lib/libmy_lib.a                             |
               | |   code_parameters:                                                 |
               | |                                                                    |
               |          \   parameters:  \  \   ./code_paramneters/parameters.xml   |
               | |         \   schema:  \  \   ./code_paramneters/parameters.xsd      |
               | |   documentation:  \  \   'Lorem ipsum dolor sit ame                |
               | t  \   ,  \  \   consectetur adipiscing elit  \   ,  \  \   sed do   |
               | |         \   eiusmod te                                             |
               | mpor incididunt ut labore et dolore magna aliqua. Ut enim ad minim   |
               | |         \   veniam  \   ,  \  \   quis                             |
               | nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo   |
               | |         \   consequat. '                                           |
               | |   language_specific:                                               |
               | |         \   compiler:  \  \   Intel                                |
               | |         \   mpi_flavour:  \  \   MPICH2                            |
               | |         \   open_mp:  \  \   false                                 |
               | |         \   system_libraries:                                      |
               | |         \   -  \  \   fftw3f                                       |
               | |         \   -  \  \   glib                                         |
               | |         \   -  \  \   mkl                                          |
               | |         \   custom_libraries:                                      |
               | |         \   -  \  \   ./lib/custom/libcustom1.a                    |
               | |         \   -  \  \   ./lib/custom/libcustom2.a                    |
               +----------------------------------------------------------------------+

            .. rubric:: 4.2.Python code description
               :name: iWrapnativecodedescription-Pythoncodedescription
               :class: auto-cursor-target

            | 

            .. container:: table-wrap

               +----------------------------------------------------------------------+
               | |   ---                                                              |
               | |   programming                                                      |
               | _language:  \  \   Python                    ! <<<<======= CHANGED   |
               | |   code_name:  \  \   demo_code                                     |
               | |   data_type:  \  \   LEGACY_IDS                                    |
               | |   arguments:                                                       |
               | |   -   name  \   :  \  \   equilibrium00                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   IN                                     |
               | |   -   name  \   :  \  \   equilibrium01                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   IN                                     |
               | |   -   name  \   :  \  \   equilibrium10                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   OUT                                    |
               | |   -   name  \   :  \  \   equilibrium11                            |
               | |         \   type:  \  \   equilibrium                              |
               | |         \   intent:  \  \   OUT                                    |
               | |   code_path:  \                                                    |
               | \   ./demo_script.py                         ! <<<<======= CHANGED   |
               | |   code_parameters:                                                 |
               | |         \   parameters:  \  \   ./code_parameters/parameters.xml   |
               | |         \   schema:  \  \   ./code_parameters/parameters.xsd       |
               | |   documentation:  \  \   'Lorem ipsum dolor sit amet, '            |
               | |   specific_se                                                      |
               | ttings:                                  \   ! <<<<======= CHANGED   |
               | |         \   language_required_version:    \   4.x                  |
               | |         \   not_sure_about_other_settings:  \  \   true            |
               +----------------------------------------------------------------------+

            | 

            | 

   .. container::
      :name: footer

      .. container:: section footer-body

         Document generated by Confluence on 27 wrz 2021 14:37

         .. container::
            :name: footer-logo

            `Atlassian <http://www.atlassian.com/>`__

.. |image1| image:: https://jira.iter.org/secure/viewavatar?size=xsmall&avatarId=13310&avatarType=issuetype
   :class: confluence-embedded-image icon confluence-external-resource
.. |(ostrzeżenie)| image:: images/icons/emoticons/warning.svg
   :class: emoticon emoticon-warning
