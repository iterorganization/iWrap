############################################################
C++ code standardisation
############################################################


Code signature
########################

.. code-block:: cpp

     #include "UALClasses.h"

     /* * * INIT/MAIN/FINALIZE method * * */
     void <method name>([IdsNs::IDS::<ids_name>& ids1, ..., IdsNs::IDS::<ids_name>& idsN,] [IdsNs::codeparam_t codeparam,] int& status_code, std::string& status_message);

     /* * * GET_STATE method * * */
    void <method name>( std::string& state_out, int& status_code, std::string& status_message);

     /* * * SET_STATE method * * */
    void <method name>( std::string state, int& status_code, std::string& status_message);

    /* * * GET_TIMESTAMP method * * */
    void <method name>(double& timestamp_out, int& status_code, std::string& status_message);

Header
########################

To generate an actor user has to provide a file containing
C++ header of wrapped method. This file can be of arbitrary
name but must contain method signature.

Method
########################

-  A user code should be provided as methods (and not a functions)
-  A name of methods could be arbitrary - chosen by code developer
-  Arguments shall be provided in a strict order
-  No INOUT arguments are allowed!

Arguments
########################

*INIT* / *MAIN* / *FINALIZE* subroutines:

-  Input and output IDSes:

   -  **Optional** arguments
   -  Input or output argument
   -  Defined as ``const IdsNs::IDS::<ids_name>`` (input) or ``IdsNs::IDS::<ids_name>&`` (output)

-  Code parameters:

   -  **Optional** argument
   -  Input argument
   -  Defined as:

      - ``std::string`` or
      - ``IdsNs::codeparam_t`` if `legacy-xml` format has been chosen for backward compatibility with older codes

        .. warning::
           Only XML parameters are passed to the code, so only ``parameters`` field
           of ``IdsNs::codeparam_t`` structure type is valid !

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

*GET_STATE subroutine:*

-  Code state:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``


*SET_STATE subroutine:*

-  Code state:

   -  **Mandatory**  argument
   -  Input argument
   -  Defined as: ``std::string``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

*GET_TIMESTAMP subroutine:*

-  Timestamp:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``double&``

-  Status code:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``int&``

-  Status message

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``std::string&``

No INOUT arguments are allowed!



Example
########################

**Header file - physics_ii.h**

.. code-block:: cpp

     #ifndef _LEVEL_II_CPP
     #define _LEVEL_II_CPP

     #include "UALClasses.h"

     /* * *   INITIALISATION method   * * */
     void init_code (std::string codeparam, int& status_code, std::string& status_message);

     /* * *   MAIN method   * * */
     void physics_ii_cpp(const IdsNs::IDS::equilibrium& in_equilibrium,
                               IdsNs::IDS::equilibrium& out_equilibrium,
                               std::string codeparam,
                               int& status_code, std::string& status_message);

     /* * *   FINALISATION method   * * */
     void clean_up(int& status_code, std::string& status_message);

     /* * * GET_STATE method * * */
    void get_code_state( std::string& state_out, int& status_code, std::string& status_message);

     /* * * SET_STATE method * * */
    void restore_code_state( std::string state, int& status_code, std::string& status_message);

     /* * * GET_TIMESTAMP method * * */
    void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message);

     #endif // _LEVEL_II_CPP

**Implementation file - level_ii.cpp**

.. code-block:: cpp

     #include "UALClasses.h"

     /* * *   INITIALISATION method   * * */
     void init_code (std::string codeparam, int& status_code, std::string& status_message)
     {
     ...
     // method body
     ...
     }

     /* * *   MAIN method   * * */
     void physics_ii_cpp(const IdsNs::IDS::equilibrium& in_equilibrium,
                               IdsNs::IDS::equilibrium& out_equilibrium,
                               std::string codeparam,
                               int& status_code, std::string& status_message)
     {
     ...
     // method body
     ...
     }

     /* * *   FINALISATION method   * * */
     void clean_up(int& status_code, std::string& status_message)
     {
     ...
     // method body
     ...
     }

     /* * * GET_STATE method * * */
    void get_code_state( std::string& state_out, int& status_code, std::string& status_message)
    {
         ...
         // method body
         ...
    }

     /* * * SET_STATE method * * */
    void restore_code_state( std::string state, int& status_code, std::string& status_message)
    {
         ...
         // method body
         ...
    }

     /* * * GET_TIMESTAMP method * * */
    void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message)
    {
         ...
         // method body
         ...
    }


Code packaging
################
A native code written in C++ should be packed within static Linux library using e.g. ar tool for that purpose.

.. code-block:: console

    ar -cr lib<name>.a <object files *.o list>
    e.g.:
    ar -cr libphysics_ii.a *.o




