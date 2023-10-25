#if 5 == AL_MAJOR
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x
    #include "ALClasses.h"
#endif

#include "code_lifecycle.h"


// =======================================
//             INITIALISATION
//=======================================

void init_code (IdsNs::codeparam_t codeparam,
                int& status_code, std::string& status_message)
{
    status_code = 0;

   status_message = "INITIALISATION: OK";

    printf("=======================================================\n");
    printf("Code lifecycle CPP: INITIALISATION called\n");
    printf("=======================================================\n");
}

// =======================================
//             FINALISATION
//=======================================
void clean_up( int& status_code, std::string& status_message)
{
    status_code = 0;
    status_message = "FINALISATION: OK";

    printf("=======================================================\n");
    printf("Code lifecycle CPP: FINALISATION called\n");
    printf("=======================================================\n");
}

// =======================================
//             MAIN
//=======================================
void code_lifecycle(const IdsNs::IDS::equilibrium& in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium,
                     IdsNs::codeparam_t xml_params,
                     int& status_code, std::string& status_message)
{
    int idsSize = -1;
    int idsTimeMode = IDS_TIME_MODE_UNKNOWN;
    
    // INITIALISATION OF ERROR FLAG
    status_code = 0;
    
    // INITIAL DISPLAY
    printf( "\n=======================================\n");
    printf( "START OF PHYSICS CODE\n");
    
    // CHECK IF INPUT IDS IS VALID
    idsSize = in_equilibrium.time.extent(0);
    idsTimeMode = in_equilibrium.ids_properties.homogeneous_time;
    if ( idsTimeMode != IDS_TIME_MODE_HOMOGENEOUS && idsSize > 0)
    {   
        // ERROR IF THE CODE DOES NOT COMPLETE TO THE END
        status_code = -1;
        status_message = "Error in code_lifecycle: input IDS not valid";
        return;
    }
    
    // MANDATORY FLAG (UNIFORM TIME HERE)
    out_equilibrium.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;
    out_equilibrium.time.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_equilibrium.time(i) = in_equilibrium.time(i);
    }

    out_equilibrium.ids_properties.homogeneous_time = 1;
    out_equilibrium.code.name   = "coreprofiles2distsource_cpp";
    out_equilibrium.code.version   = "1.0";
    out_equilibrium.code.parameters = "my_code_specific_parameters";
    out_equilibrium.code.output_flag.resize(1);
    out_equilibrium.code.output_flag(0) = 0;

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of code_lifecycle CPP";

    
    // FINAL DISPLAY
    printf( "END OF PHYSICS CODE\n");
    printf( "=======================================\n");
    printf( " \n");
}
