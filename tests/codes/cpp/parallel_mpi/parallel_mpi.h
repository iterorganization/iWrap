#ifndef _PARALLEL_MPI_CPP
#define _PARALLEL_MPI_CPP

#include "ALClasses.h"

void codeStep(const IdsNs::IDS::core_profiles& core_profiles_in,
                         IdsNs::IDS::distribution_sources& distribution_sources_out,
                         int& status_code, std::string& status_message);

#endif
