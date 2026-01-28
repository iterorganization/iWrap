#ifndef _CODE_RESTART_CPP
#define _CODE_RESTART_CPP

#if 5 == AL_MAJOR
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x
    #include "ALClasses.h"
#endif

void get_code_state( std::string& state_out, int& status_code, std::string& status_message);

void restore_code_state( std::string state, int& status_code, std::string& status_message);

void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message);


void init_code (int& status_code, std::string& status_message);
void clean_up( int& status_code, std::string& status_message);

void code_step(const IdsNs::IDS::core_profiles& core_profiles_in,
                    IdsNs::IDS::distribution_sources& distribution_sources_out,
                    int& status_code, std::string& status_message);

#endif // _CODE_RESTART_CPP