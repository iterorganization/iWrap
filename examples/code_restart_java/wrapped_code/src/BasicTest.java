package iwrap.test_codes.basic;

import imasjava.Vect1DDouble;
import imasjava.imas;

import static imasjava.wrapper.LowLevel.IDS_TIME_MODE_HOMOGENEOUS;

public class BasicTest {

    private int code_state = 0;

    // =======================================
    //             INITIALISATION
    //=======================================
    public void init_code() throws Exception
    {
        this.init_code(null);
    }

    public void init_code (String code_parameters) throws Exception
    {
        System.out.println("=======================================================");
        System.out.println("Java code: INITIALISATION called");
        System.out.println("=======================================================");
    }

    // =======================================
    //             FINALISATION
    //=======================================
    public void clean_up( ) throws Exception
    {
        System.out.println("=======================================================");
        System.out.println("Java code: FINALISATION called");
        System.out.println("=======================================================");
    }

    // =======================================
    //             MAIN
    //=======================================
    public Object[] code_step(imas.core_profiles  core_profiles_in) throws Exception
    {
         return this.code_step(core_profiles_in, null);
    }

    public Object[] code_step(imas.core_profiles  core_profiles_in,
                          String codeParameters) throws Exception
    {

        imas.distribution_sources  distribution_sources_out = new imas.distribution_sources();
        int inIdsSize = -1;
        Object[] retArray = new Object[1];

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

        distribution_sources_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;

        inIdsSize = core_profiles_in.time.getDim();
        System.out.println( "Input IDS size : " + inIdsSize);

        if (inIdsSize > 0) {

            distribution_sources_out.time = new Vect1DDouble(inIdsSize);

            // Time : copy from input IDS
            for (int i = 0; i < inIdsSize; i++) {
                double newValue = 1000 * code_state + core_profiles_in.time.getElementAt(i);
                distribution_sources_out.time.setElementAt(i, newValue);
            }
        }
        else {
            distribution_sources_out.time = new Vect1DDouble(inIdsSize);
            double newValue = 1000 * code_state ;
            distribution_sources_out.time.setElementAt(0, newValue);
        }


        // FINAL DISPLAY
        System.out.println( "END OF PHYSICS CODE");
        System.out.println( "=======================================");
        System.out.println( " ");

        retArray[0] = distribution_sources_out;
        return retArray;
    }

    // =======================================
    //             GET STATE
    //=======================================
    public String get_code_state() throws Exception
    {
        String state_out = String.valueOf(code_state);

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
