#ifndef _PARALLEL_MPI_CPP
#define _PARALLEL_MPI_CPP

#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

void codeStep(const IdsNs::IDS::core_profiles& core_profiles_in,
                         IdsNs::IDS::distribution_sources& distribution_sources_out,
                         int& status_code, std::string& status_message);

#endif
