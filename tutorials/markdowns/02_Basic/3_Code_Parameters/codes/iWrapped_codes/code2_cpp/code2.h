#ifndef _CODE1_CPP
#define _CODE1_CPP

#include "ALClasses.h"

using namespace IdsNs;

// INIT
void code2_setup(codeparam_t code_parameters, int& status_code, std::string& status_message);

// MAIN
void code2_step(const IDS::distribution_sources ids_in,
                      IDS::core_profiles& ids_out,
                      codeparam_t code_parameters, 
                      int& status_code, std::string& status_message);

// FINALIZATION
void code2_cleanup(int& status_code, std::string& status_message);

#endif // _CODE1_CPP