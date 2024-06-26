#include "basic_methods_cpp.h"


// =======================================
//             INITIALISATION
//=======================================

void init_code_cpp (const IdsNs::IDS::core_profiles& in_core_profiles,
                          IdsNs::IDS::distribution_sources& out_distribution_sources,
                          IdsNs::codeparam_t codeparam,
                          int& status_code, std::string& status_message)
{
    int idsSize = -1;
    status_code = 0;

    status_message = "Basic methods CPP: INIT: OK";

    printf("=======================================================\n");
    printf("Basic methods CPP: INIT starts\n");
    printf("-------------------------------------------------------\n");

    idsSize = in_core_profiles.time.extent(0);
    printf("Size of input IDS  = %d\n", idsSize);

    out_distribution_sources.time.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_distribution_sources.time(i) = in_core_profiles.time(i);
    }

    out_distribution_sources.ids_properties.homogeneous_time = 1;
    out_distribution_sources.code.name   = "basic_methods_cpp";
    out_distribution_sources.code.version   = "1.0";

    printf("-------------------------------------------------------\n");
    printf("Basic methods CPP: INIT ends\n");
    printf("=======================================================\n");
}

// =======================================
//             FINALISATION
//=======================================
void clean_up_cpp(const IdsNs::IDS::distribution_sources& in_distribution_sources,
                        IdsNs::IDS::core_profiles& out_core_profiles,
                        int& status_code, std::string& status_message)
{
    int idsSize = -1;

    status_code = 0;
    status_message = "FINALISATION: OK";
    printf("=======================================================\n");
    printf("Basic methods CPP: FINALISATION starts\n");
    printf("-------------------------------------------------------\n");

    idsSize = in_distribution_sources.time.extent(0);
    printf("Size of input IDS  = %d\n", idsSize);

    out_core_profiles.time.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_core_profiles.time(i) = in_distribution_sources.time(i);
    }

    out_core_profiles.ids_properties.homogeneous_time = 1;
    out_core_profiles.code.name   = "basic_methods_cpp";
    out_core_profiles.code.version   = "1.0";

    printf("-------------------------------------------------------\n");
    printf("Basic methods CPP: FINALISATION ends\n");
    printf("=======================================================\n");
}

// =======================================
//             MAIN
//=======================================
void step_cpp(const IdsNs::IDS::equilibrium& in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium,
                     IdsNs::codeparam_t xml_params,
                     int& status_code, std::string& status_message)
{
    int idsSize = -1;
    int idsTimeMode = IDS_TIME_MODE_UNKNOWN;
    
    // INITIALISATION OF ERROR FLAG
    status_code = 0;
    
    // INITIAL DISPLAY

    printf("=======================================================\n");
    printf("Basic methods CPP: MAIN starts\n");
    printf("-------------------------------------------------------\n");

    // CHECK IF INPUT IDS IS VALID
    idsSize = in_equilibrium.time.extent(0);
    idsTimeMode = in_equilibrium.ids_properties.homogeneous_time;
    if ( idsTimeMode != IDS_TIME_MODE_HOMOGENEOUS && idsSize > 0)
    {   
        // ERROR IF THE CODE DOES NOT COMPLETE TO THE END
        status_code = -1;
        status_message = "Error in basic_methods_cpp: input IDS not valid";
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
    out_equilibrium.code.name   = "basic_methods_cpp";
    out_equilibrium.code.version   = "1.0";
    out_equilibrium.code.output_flag.resize(1);
    out_equilibrium.code.output_flag(0) = 0;

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of basic_methods_cpp CPP";

    
    // FINAL DISPLAY
    printf("-------------------------------------------------------\n");
    printf("Basic methods CPP: MAIN ends\n");
    printf("=======================================================\n");
}
