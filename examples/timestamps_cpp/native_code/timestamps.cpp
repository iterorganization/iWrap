#include "UALClasses.h"


double timestamp = 0;

void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message)
{
    timestamp_out = timestamp;
}

void timestamps_cpp(const IdsNs::IDS::core_profiles& in_core_profiles,
                                       IdsNs::IDS::distribution_sources& out_distribution_sources,
                                       int& status_code, std::string& status_message)
{
    int idsSize = -1;

    printf("Entering subroutine timestamps_cpp\n");
    status_code = 0;

    idsSize = in_core_profiles.time.extent(0);

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of timestamps_cpp CPP";

    out_distribution_sources.time.resize(idsSize);



    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_distribution_sources.time(i) = in_core_profiles.time(i);
    }

    out_distribution_sources.ids_properties.homogeneous_time = 1;

    timestamp ++;

    printf("End of timestamps_cpp \n");

}