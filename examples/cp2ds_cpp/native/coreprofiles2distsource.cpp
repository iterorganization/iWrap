#include "UALClasses.h"


void coreprofiles2distsource_cpp(IdsNs::IDS::core_profiles& in_core_profiles, IdsNs::IDS::distribution_sources& out_distribution_sources, int* status_code, char** status_message)
{
    const char* TEXT = "Status info of coreprofiles2distsource CPP";
    int idsSize = -1;

    printf("Entering subroutine eq2dist");
    idsSize = in_core_profiles.time.extent(0);

    printf("size of input IDS  = .d", idsSize);

    // INITIALISATION OF STATUS INFO
    *status_message = (char*)malloc(strlen(TEXT) + 1);
    strcpy (*status_message, TEXT);

    out_distribution_sources.time.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        printf("Received input time from equilibrium : %f", in_core_profiles.time(i));
        out_distribution_sources.time(i) = in_core_profiles.time(i);
    }

    out_distribution_sources.ids_properties.homogeneous_time = 1;
    out_distribution_sources.code.name   = "coreprofiles2distsource_cpp";
    out_distribution_sources.code.version   = "1.0";
    out_distribution_sources.code.parameters = "my_code_specific_parameters";
    out_distribution_sources.code.output_flag = 0   ;

    *status_code = 0;
    printf("End of coreprofiles2distsource CPP");

}