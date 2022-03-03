#ifndef _CODE_LIFECYCLE_CPP
#define _CODE_LIFECYCLE_CPP

#include "UALClasses.h"

void InitCode (IdsNs::codeparam_t codeparam,
                int& status_code, std::string& status_message);

void CleanUp( int& status_code, std::string& status_message);

void UpperCase(const IdsNs::IDS::equilibrium& in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium,
                     IdsNs::codeparam_t xml_params,
                     int& status_code, std::string& status_message);

#endif // _CODE_LIFECYCLE_CPP