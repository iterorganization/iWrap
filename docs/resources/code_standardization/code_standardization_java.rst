############################################################
Java code standardisation
############################################################

Code signature
########################

.. code-block:: Java

    package <package name>;

    import imasjava.imas;

    public class <class name> {

        /* * * INIT / MAIN / FINALIZE method * * */
        public Object[] <method name>(imas.<ids_type> ids)_in1, ..., imas.<ids_type> ids_inN[, String codeParameters]) throws Exception { ... }

        /* * * GET_STATE method * * */
        public String <method name>() throws Exception { ... }

        /* * * SET_STATE method * * */
        public void <method name>( String state ) throws Exception { ... }

        /* * * GET_TIMESTAMP method * * */
        public double <method name>() throws Exception { ... }
    }

Methods
########################

-  A name of package, class and methods could be arbitrary - chosen by code developer
-  Access modifiers for both: class and methods shall be set to `public` to make them accessible for a wrapper
-  Arguments shall be provided in a strict order
-  All errors from the native code shall be passed using exceptions
-  No INOUT arguments are allowed!

Arguments
########################

*INIT* / *MAIN* / *FINALIZE* subroutines:

-  Input IDSes:

   -  **Optional** arguments
   -  Input argument
   -  Defined as ``imas.<ids_type>``


-  XML parameters:

   -  **Optional** argument
   -  Input argument
   -  Defined as ``String``

-  Returns: output IDSes:

   -  Output arguments
   -  Defined as an array ``Object[]``
   -  Elements of the array should be of ``imas.<ids_name>`` type


*GET_STATE subroutine:*

-  Returned: Code state:
   -  **Mandatory**  argument
   -  Intent: OUT
   -  Defined as ``String``

*SET_STATE subroutine:*

-  Code state:

   -  **Mandatory**  argument
   -  Input argument
   -  Defined as: ``String``


*GET_TIMESTAMP subroutine:*

-  Timestamp:

   -  **Mandatory**  argument
   -  Output argument
   -  Defined as: ``double``



Example
########################

.. code-block:: java

    public class BasicTest {

        /* * * INIT method * * */
        public void init_code (String code_parameters) throws Exception
        {
            System.out.println("Java code: INITIALISATION called");
             ...
             // method body
             ...
        }

        /* * * MAIN method * * */
        public void code_step(imas.core_profiles  core_profiles_in,
                              String codeParameters) throws Exception
        {
             imas.distribution_sources  ds1_out = new imas.distribution_sources();
             imas.distribution_sources  ds2_out = new imas.distribution_sources();

             Object retArray = new Object[2];

             System.out.println("Java code: MAIN called");
             ...
             // method body
             ...
            retArray[0] = ds1_out;
            retArray[1] = ds2_out;

            return retArray;
        }

        /* * * FINALIZE method * * */
        public void clean_up( ) throws Exception
        {
            System.out.println("Java code: FINALISATION called");
        }

        /* * * GET_STATE method * * */
        public String get_code_state() throws Exception
        {
            String state_out = ....;

            System.out.println("Java code: GET STATE called");
            return state_out;
        }

        /* * * SET_STATE method * * */
        public void restore_code_state( String state ) throws Exception
        {
            this.code_state = state;

            System.out.println("Java code: SET STATE called");
        }

        /* * * GET_TIMESTAMP method * * */
        public double  get_timestamp_cpp() throws Exception
        {
            double timestamp_out;

            System.out.println("Java code: GET TIMESTAMP called");
            timestamp_out = .....;
            return timestamp_out;
        }
    }

Code packaging
################
A code should be compiled and packed within Java archive (JAR).

.. code-block:: console

	javac <source files> -d <build dir>
	jar cf <archive name>>.jar  -C <build dir> .

    e.g.

	mkdir -p build
	javac src/*.java -d build/
	jar cf basic_test.jar  -C build/ .

Dependencies
################
*Java* must be available in the system to generate and build an actor.
To run it, beside *Java*, *JPype* Python package is utilised.

