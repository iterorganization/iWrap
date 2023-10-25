#ifndef _COREPROFILES2DISTSOURCE_CPP
#define _COREPROFILES2DISTSOURCE_CPP

#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

void cp2ds_mpi_cpp(const IdsNs::IDS::core_profiles& in_core_profiles,
                         IdsNs::IDS::distribution_sources& out_distribution_sources,
                         int& status_code, std::string& status_message);

#endif // _COREPROFILES2DISTSOURCE_CPP
