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
//             STATE FUNCTIONS
// =======================================
void getCodeState(std::string& state_out, int& status_code, std::string& status_message);
void restoreCodeState(std::string state, int& status_code, std::string& status_message);

// =======================================
//           TIMESTAMP FUNCTIONS
// =======================================
void getTimestampCpp(double& timestamp_out, int& status_code, std::string& status_message);

// =======================================
//             INIT FUNCTIONS
// =======================================
void initCode(int& status_code, std::string& status_message);

void initCode(std::string parameters_str, int& status_code, std::string& status_message);

void initCode(const IdsNs::IDS::core_profiles& in_core_profiles,
                          IdsNs::IDS::distribution_sources& out_distribution_sources,
                          std::string parameters_str,
                          int& status_code, std::string& status_message);

void initCode(const IdsNs::IDS::core_profiles& in_core_profiles,
                          IdsNs::IDS::distribution_sources& out_distribution_sources,
                          int& status_code, std::string& status_message);

// =======================================
//        LEGACY-XML INIT FUNCTIONS
// =======================================

void initCode(IdsNs::codeparam_t codeparam, int& status_code, std::string& status_message);

void initCode(const IdsNs::IDS::core_profiles& in_core_profiles,
                          IdsNs::IDS::distribution_sources& out_distribution_sources,
                          IdsNs::codeparam_t codeparam,
                          int& status_code, std::string& status_message);

// =======================================
//          FINALIZE FUNCTIONS
// =======================================

void cleanUp(int& status_code, std::string& status_message);

void cleanUp(const IdsNs::IDS::distribution_sources& distribution_sources_in,
                        IdsNs::IDS::core_profiles& core_profiles_out,
                        int& status_code, std::string& status_message);

void cleanUp(std::string parameters_str, int& status_code, std::string& status_message);

void cleanUp(const IdsNs::IDS::distribution_sources& distribution_sources_in,
                        IdsNs::IDS::core_profiles& core_profiles_out,
                        std::string parameters_str,
                        int& status_code, std::string& status_message);

// =======================================
//      LEGACY-XML FINALIZE FUNCTIONS
// =======================================

void cleanUp(IdsNs::codeparam_t codeparam, int& status_code, std::string& status_message);

void cleanUp(const IdsNs::IDS::distribution_sources& distribution_sources_in,
                        IdsNs::IDS::core_profiles& core_profiles_out,
                        IdsNs::codeparam_t codeparam,
                        int& status_code, std::string& status_message);

// =======================================
//             MAIN FUNCTIONS
// =======================================

void codeStep(int& status_code, std::string& status_message);

void codeStep(std::string parameters_str, int& status_code, std::string& status_message);

void codeStep(const IdsNs::IDS::equilibrium& equilibrium_in, IdsNs::IDS::equilibrium& equilibrium_out,
                     std::string parameters_str,
                     int& status_code, std::string& status_message);

void codeStep(const IdsNs::IDS::equilibrium& equilibrium_in, IdsNs::IDS::equilibrium& equilibrium_out,
                     int& status_code, std::string& status_message);

// =======================================
//        LEGACY-XML MAIN FUNCTIONS
// =======================================

void codeStep(IdsNs::codeparam_t codeparam, int& status_code, std::string& status_message);

void codeStep(const IdsNs::IDS::equilibrium& equilibrium_in, IdsNs::IDS::equilibrium& equilibrium_out,
                     IdsNs::codeparam_t codeparam,
                     int& status_code, std::string& status_message);

#endif // _BASIC_METHODS_CPP