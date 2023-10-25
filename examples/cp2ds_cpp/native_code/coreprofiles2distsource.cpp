#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif


void coreprofiles2distsource_cpp(const IdsNs::IDS::core_profiles& in_core_profiles,
                                       IdsNs::IDS::distribution_sources& out_distribution_sources,
                                       int& status_code, std::string& status_message)
{
    int idsSize = -1;

    printf("Entering subroutine eq2dist\n");
    status_code = 0;

    idsSize = in_core_profiles.time.extent(0);

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of coreprofiles2distsource CPP";

    out_distribution_sources.time.resize(idsSize);



    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_distribution_sources.time(i) = in_core_profiles.time(i);
    }

    out_distribution_sources.ids_properties.homogeneous_time = 1;
    out_distribution_sources.code.name   = "coreprofiles2distsource_cpp";
    out_distribution_sources.code.version   = "1.0";
    out_distribution_sources.code.parameters = "my_code_specific_parameters";
    out_distribution_sources.code.output_flag = 0   ;


    printf("End of coreprofiles2distsource CPP\n");

}
