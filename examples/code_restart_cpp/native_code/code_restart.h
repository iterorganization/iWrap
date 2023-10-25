#ifndef _CODE_RESTART_CPP
#define _CODE_RESTART_CPP

#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

void get_code_state( std::string& state_out, int& status_code, std::string& status_message);

void restore_code_state( std::string state, int& status_code, std::string& status_message);

void code_restart(const IdsNs::IDS::equilibrium& in_equilibrium,
                     int& status_code, std::string& status_message);

#endif // _CODE_RESTART_CPP