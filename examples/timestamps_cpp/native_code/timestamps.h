#ifndef _COREPROFILES2DISTSOURCE_CPP
#define _COREPROFILES2DISTSOURCE_CPP

#include "UALClasses.h"

void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message);



void timestamps_cpp(const IdsNs::IDS::core_profiles& in_core_profiles,
                                       IdsNs::IDS::distribution_sources& out_distribution_sources,
                                       int& status_code, std::string& status_message);

#endif // _COREPROFILES2DISTSOURCE_CPP