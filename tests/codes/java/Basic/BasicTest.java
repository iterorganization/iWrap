package iwrap.test_codes.basic;

import imasjava.Vect1DDouble;
import imasjava.Vect1DInt;
import imasjava.imas;

import static imasjava.wrapper.LowLevel.IDS_TIME_MODE_HOMOGENEOUS;


public class BasicTest
{

    private int code_state = 1;
    private double timestamp = 0.0;

    // ================================================================================================================
    //                                           INIT SUBROUTINES
    // ================================================================================================================

    public Object[] initCode()
    {
        return this.initCode(null, null);
    }

    public Object[] initCode (String code_parameters)
    {
        return this.initCode(null, code_parameters);
    }

    public Object[] initCode (imas.core_profiles  core_profiles_in)
    {
        return this.initCode(core_profiles_in, null);
    }

    public Object[] initCode (imas.core_profiles  core_profiles_in, String code_parameters)
    {
        imas.distribution_sources  distribution_sources_out = new imas.distribution_sources();
        Object[] retArray = new Object[1];

        int inIdsSize = -1;

        System.out.println("=======================================================");
        System.out.println("Java code: INITIALISATION called");

        distribution_sources_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;

        inIdsSize = core_profiles_in.time.getDim();
        System.out.println( "Input IDS size : " + inIdsSize);

        if (inIdsSize > 0)
        {
            distribution_sources_out.time = new Vect1DDouble(inIdsSize);

            // Time : copy from input IDS
            for (int i = 0; i < inIdsSize; i++)
            {
                double newValue = 1000 + core_profiles_in.time.getElementAt(i);
                distribution_sources_out.time.setElementAt(i, newValue);
            }
        }
        else
        {
            distribution_sources_out.time = new Vect1DDouble(inIdsSize);
            double newValue = 1000 * code_state;
            distribution_sources_out.time.setElementAt(0, newValue);
        }

        System.out.println("=======================================================");

        retArray[0] = distribution_sources_out;
        return retArray;
    }

    // ================================================================================================================
    //                                           MAIN SUBROUTINES
    // ================================================================================================================
    public Object[] codeStep(imas.equilibrium  equilibrium_in)
    {
         return this.codeStep(equilibrium_in, null);
    }

    public Object[] codeStep(imas.equilibrium equilibrium_in, String codeParameters)
    {
        imas.equilibrium  equilibrium_out;
        Object[] retArray = new Object[1];

        int ntimes = 1;
        Float multiplication_factor = java.lang.Float.valueOf(1);

        int inIdsSize = -1;

        // INITIAL DISPLAY
        System.out.println( "=======================================");
        System.out.println( "START OF PHYSICS CODE");

        if(codeParameters != null)
        {
            System.out.println( "Got code parameters: ");
            try
            {
                Number[] extracted_parameters = ParametersUtils.extract_values(codeParameters);
                ntimes = (int)extracted_parameters[0];
                multiplication_factor = extracted_parameters[1].floatValue();
                System.out.println( "ntimes               : " + ntimes);
                System.out.println( "multiplication_factor: " + multiplication_factor);
                System.out.println( "-------------------------------------");
            }
            catch(Exception e)
            {
                System.out.println( "ERROR LOADING PARAMETERS: " + e);
            }
        }

        equilibrium_out = new imas.equilibrium();
        equilibrium_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;

        inIdsSize = equilibrium_in.time.getDim();
        System.out.println( "Input IDS size : " + inIdsSize);

        if (inIdsSize > 0)
        {
            equilibrium_out.time = new Vect1DDouble(inIdsSize);

            // Time : copy from input IDS
            for (int i = 0; i < inIdsSize; i++)
            {
                double newValue = equilibrium_in.time.getElementAt(i);
                equilibrium_out.time.setElementAt(i, newValue);
            }

            // MODIFY OUTPUT FLAG
            equilibrium_out.code.output_flag = new Vect1DInt(inIdsSize);
            equilibrium_out.code.output_flag.setElementAt(0, (int)(equilibrium_in.time.getElementAt(0) * ntimes * multiplication_factor.floatValue() * code_state));
        }
        else
        {
            equilibrium_out.time = new Vect1DDouble(inIdsSize);
            double newValue = 1000 * code_state;
            equilibrium_out.time.setElementAt(0, newValue);
        }

        code_state++;
        timestamp = timestamp + 10;

        // FINAL DISPLAY
        System.out.println( "END OF PHYSICS CODE");
        System.out.println( "=======================================");
        System.out.println( " ");

        retArray[0] = equilibrium_out;
        return retArray;
    }

// ================================================================================================================
//                                           FINALIZE SUBROUTINES
// ================================================================================================================
    public Object[] cleanUp() throws Exception
    {
        return this.cleanUp(null, null);
    }

    public Object[] cleanUp (String code_parameters) throws Exception
    {
        return this.cleanUp(null, code_parameters);
    }

    public Object[] cleanUp (imas.distribution_sources distribution_sources_in) throws Exception
    {
        return this.cleanUp(distribution_sources_in, null);
    }

    public Object[] cleanUp (imas.distribution_sources distribution_sources_in, String code_parameters) throws Exception
    {
        int inIdsSize = -1;
        Object[] retArray = new Object[1];
        imas.core_profiles core_profiles_out = new imas.core_profiles();

        System.out.println("=======================================================");
        System.out.println("Java code: FINALISATION called");

        core_profiles_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;

        inIdsSize = distribution_sources_in.time.getDim();
        System.out.println( "Input IDS size : " + inIdsSize);

        if (inIdsSize > 0)
        {
            core_profiles_out.time = new Vect1DDouble(inIdsSize);

            // Time : copy from input IDS
            for (int i = 0; i < inIdsSize; i++)
            {
                double newValue = 1000 + distribution_sources_in.time.getElementAt(i);
                core_profiles_out.time.setElementAt(i, newValue);
            }
        }
        else
        {
            core_profiles_out.time = new Vect1DDouble(inIdsSize);
            double newValue = 1;
            core_profiles_out.time.setElementAt(0, newValue);
        }
        System.out.println("=======================================================");

        retArray[0] = core_profiles_out;
        return retArray;
    }

    // =====================================================================================
    //             GET STATE
    // =====================================================================================
    public String getCodeState() throws Exception
    {
        String state_out = String.valueOf(this.code_state);

        System.out.println("=======================================================");
        System.out.println("Java code: GET STATE called");
        System.out.println("STATE is : " + state_out);
        System.out.println("=======================================================");
        return state_out;
    }

    // =====================================================================================
    //             SET STATE
    // =====================================================================================
    public void restoreCodeState( String state ) throws Exception
    {
        this.code_state = Integer.parseInt( state );

        System.out.println("=======================================================");
        System.out.println("Java code: RESTORE STATE called");
        System.out.println("STATE TO BE RESTORED : " + this.code_state);
        System.out.println("=======================================================");
    }

    // =====================================================================================
    //             GET TIMESTAMP
    // =====================================================================================
    public double getTimestamp() throws Exception
    {
        System.out.println("=======================================================");
        System.out.println("Java code: GET TIMESTAMP called");
        System.out.println("=======================================================");

        return this.timestamp;
    }
}
