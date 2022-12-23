#ifndef _CODE_RESTART_CPP
#define _CODE_RESTART_CPP

#include "UALClasses.h"

void get_code_state( std::string& state_out, int& status_code, std::string& status_message);

void restore_code_state( std::string state, int& status_code, std::string& status_message);

void code_restart(const IdsNs::IDS::equilibrium& in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium,
                     IdsNs::codeparam_t xml_params,
                     int& status_code, std::string& status_message);

#endif // _CODE_RESTART_CPP