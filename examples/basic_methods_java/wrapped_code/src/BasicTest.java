package iwrap.test_codes.basic;

import imasjava.Vect1DDouble;
import imasjava.imas;

import static imasjava.wrapper.LowLevel.IDS_TIME_MODE_HOMOGENEOUS;

public class BasicTest {

    private int code_state = 0;

    // =======================================
    //             INITIALISATION
    //=======================================

    public void init_code()
    {
        this.init_code(null);
    }

    public void init_code (String code_parameters)
    {
        this.init_code(null, null, code_parameters);
    }

        public void init_code (imas.core_profiles  core_profiles_in,
                          imas.distribution_sources  distribution_sources_out)
    {
        this.init_code(core_profiles_in, distribution_sources_out, null);
    }

    public void init_code (imas.core_profiles  core_profiles_in,
                          imas.distribution_sources  distribution_sources_out,
                          String code_parameters)
    {
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
    }



    // =======================================
    //             MAIN
    //=======================================
    public void code_step(imas.distribution_sources  distribution_sources_in,
                          imas.core_profiles  core_profiles_out)
    {
         this.code_step(distribution_sources_in, core_profiles_out, null);
    }

    public void code_step(imas.distribution_sources  distribution_sources_in,
                          imas.core_profiles  core_profiles_out,
                          String codeParameters)
    {
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
    }

    // =======================================
    //             FINALISATION
    //=======================================
    public void clean_up() throws Exception
    {
        this.clean_up(null);
    }

    public void clean_up (String code_parameters) throws Exception
    {
        this.clean_up(null, null, code_parameters);
    }

    public void clean_up (imas.core_profiles  core_profiles_in,
                          imas.distribution_sources  distribution_sources_out) throws Exception
    {
        this.clean_up(core_profiles_in, distribution_sources_out, null);
    }

    public void clean_up (imas.core_profiles  core_profiles_in,
                          imas.distribution_sources  distribution_sources_out,
                          String code_parameters) throws Exception
    {
        int inIdsSize = -1;

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
