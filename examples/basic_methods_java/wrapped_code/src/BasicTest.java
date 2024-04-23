package iwrap.test_codes.basic;

import imasjava.Vect1DDouble;
import imasjava.imas;

import static imasjava.wrapper.LowLevel.IDS_TIME_MODE_HOMOGENEOUS;

public class BasicTest {

    private int code_state = 0;

    // =======================================
    //             INITIALISATION
    //=======================================

    public Object[] init_code()
    {
        return this.init_code(null, null);
    }

    public Object[] init_code (String code_parameters)
    {
        return this.init_code(null, code_parameters);
    }

        public Object[] init_code (imas.core_profiles  core_profiles_in)
    {
        return this.init_code(core_profiles_in, null);
    }

    public Object[] init_code (imas.core_profiles  core_profiles_in,
                          String code_parameters)
    {
       imas.distribution_sources  distribution_sources_out = new imas.distribution_sources();
       Object[] retArray = new Object[1];

        int inIdsSize = -1;

        System.out.println("=======================================================");
        System.out.println("Java code: INITIALISATION called");

        distribution_sources_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;

        inIdsSize = core_profiles_in.time.getDim();
        System.out.println( "Input IDS size : " + inIdsSize);

        if (inIdsSize > 0) {

            distribution_sources_out.time = new Vect1DDouble(inIdsSize);

            // Time : copy from input IDS
            for (int i = 0; i < inIdsSize; i++) {
                double newValue = 1000 + core_profiles_in.time.getElementAt(i);
                distribution_sources_out.time.setElementAt(i, newValue);
            }
        }
        else {
            distribution_sources_out.time = new Vect1DDouble(inIdsSize);
            double newValue = 1000 * code_state ;
            distribution_sources_out.time.setElementAt(0, newValue);
        }

        System.out.println("=======================================================");

        retArray[0] = distribution_sources_out;
        return retArray;
    }



    // =======================================
    //             MAIN
    //=======================================
    public Object[] code_step(imas.distribution_sources  distribution_sources_in)
    {
         return this.code_step(distribution_sources_in, null);
    }

    public Object[] code_step(imas.distribution_sources  distribution_sources_in,
                          String codeParameters)
    {
        imas.core_profiles  core_profiles_out;
        Object[] retArray = new Object[1];


        int inIdsSize = -1;


        // INITIAL DISPLAY
        System.out.println( "=======================================");
        System.out.println( "START OF PHYSICS CODE");

        if(codeParameters != null)
        {
            System.out.println( "Got code parameters: ");
            System.out.println( codeParameters);
            System.out.println( "-------------------------------------");
        }
        System.out.println( "Starting from: " + code_state);

        for (int i = 0; i < 20; i++)
        {
            // COMPUTATIONS
            code_state++;
        }

        System.out.println( "Counting to : " + code_state);

        core_profiles_out = new imas.core_profiles();
        core_profiles_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;

        inIdsSize = distribution_sources_in.time.getDim();
        System.out.println( "Input IDS size : " + inIdsSize);

        if (inIdsSize > 0) {

            core_profiles_out.time = new Vect1DDouble(inIdsSize);

            // Time : copy from input IDS
            for (int i = 0; i < inIdsSize; i++) {
                double newValue = 1000 * code_state + distribution_sources_in.time.getElementAt(i);
                core_profiles_out.time.setElementAt(i, newValue);
            }
        }
        else {
            core_profiles_out.time = new Vect1DDouble(inIdsSize);
            double newValue = 1000 * code_state ;
            core_profiles_out.time.setElementAt(0, newValue);
        }


        // FINAL DISPLAY
        System.out.println( "END OF PHYSICS CODE");
        System.out.println( "=======================================");
        System.out.println( " ");

        retArray[0] = core_profiles_out;
        return retArray;
    }

    // =======================================
    //             FINALISATION
    //=======================================
    public Object[]  clean_up() throws Exception
    {
        return this.clean_up(null, null);
    }

    public Object[]  clean_up (String code_parameters) throws Exception
    {
        return this.clean_up(null, code_parameters);
    }

    public Object[]  clean_up (imas.core_profiles  core_profiles_in) throws Exception
    {
        return this.clean_up(core_profiles_in, null);
    }

    public Object[]  clean_up (imas.core_profiles  core_profiles_in,
                          String code_parameters) throws Exception
    {
        int inIdsSize = -1;
        Object[] retArray = new Object[1];
        imas.distribution_sources  distribution_sources_out = new imas.distribution_sources();

        System.out.println("=======================================================");
        System.out.println("Java code: FINALISATION called");

        distribution_sources_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;

        inIdsSize = core_profiles_in.time.getDim();
        System.out.println( "Input IDS size : " + inIdsSize);

        if (inIdsSize > 0) {

            distribution_sources_out.time = new Vect1DDouble(inIdsSize);

            // Time : copy from input IDS
            for (int i = 0; i < inIdsSize; i++) {
                double newValue = 1000 + core_profiles_in.time.getElementAt(i);
                distribution_sources_out.time.setElementAt(i, newValue);
            }
        }
        else {
            distribution_sources_out.time = new Vect1DDouble(inIdsSize);
            double newValue = 1;
            distribution_sources_out.time.setElementAt(0, newValue);
        }
        System.out.println("=======================================================");

        retArray[0] = distribution_sources_out;
        return retArray;
    }



    // =======================================
    //             GET STATE
    //=======================================
    public String get_code_state() throws Exception
    {
        String state_out = String.valueOf(this.code_state);

        System.out.println("=======================================================");
        System.out.println("Java code: GET STATE called");
        System.out.println("STATE is : " + state_out);
        System.out.println("=======================================================");
        return state_out;
    }

    // =======================================
    //             SET STATE
    //=======================================
    public void restore_code_state( String state ) throws Exception
    {
        this.code_state = Integer.parseInt( state );

        System.out.println("=======================================================");
        System.out.println("Java code: RESTORE STATE called");
        System.out.println("STATE TO BE RESTORED : " + this.code_state);
        System.out.println("=======================================================");
    }

    // =======================================
    //             GET TIMESTAMP
    //=======================================
    public double  get_timestamp() throws Exception
    {
        double timestamp_out;

        System.out.println("=======================================================");
        System.out.println("Java code: GET TIMESTAMP called");
        System.out.println("=======================================================");

        timestamp_out = (double) code_state;
        return timestamp_out;
    }
}
