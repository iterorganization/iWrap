#ifndef _BASIC_METHODS_CPP
#define _BASIC_METHODS_CPP

#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

// =======================================
//             INITIALISATION
//=======================================
void init_code_cpp (const IdsNs::IDS::core_profiles& in_core_profiles,
                          IdsNs::IDS::distribution_sources& out_distribution_sources,
                          IdsNs::codeparam_t codeparam,
                          int& status_code, std::string& status_message);


// =======================================
//             FINALISATION
//=======================================
void clean_up_cpp(const IdsNs::IDS::distribution_sources& in_distribution_sources,
                        IdsNs::IDS::core_profiles& out_core_profiles,
                        int& status_code, std::string& status_message);

// =======================================
//             MAIN
//=======================================
void step_cpp(const IdsNs::IDS::equilibrium& in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium,
                     IdsNs::codeparam_t xml_params,
                     int& status_code, std::string& status_message);

#endif // _BASIC_METHODS_CPP
