#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

#include "Basic.h"
#include "ParametersUtils.h"
#include "json/json.h"
#include "xmlIMAS.h"
#include <boost/algorithm/string.hpp>
#include <memory>
#include <stdexcept>

// ================================================================================================================
//                                           CODE STATE SUBROUTINES
// ================================================================================================================

// =======================================
//             GET STATE
// =======================================

int code_state = 1;
double timestamp = 0.0;

void getCodeState( std::string& state_out, int& status_code, std::string& status_message)
{
    status_code = 0;

    status_message = "INITIALISATION: OK";
    state_out = std::to_string(code_state);

    std::cout << "=======================================================" << std::endl;
    std::cout << "Basic CPP: GET STATE called" << std::endl;
    std::cout << "STATE is : " << state_out << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             SET STATE
// =======================================
void restoreCodeState( std::string state, int& status_code, std::string& status_message)
{
    status_code = 0;
    status_message = "FINALISATION: OK";

    code_state = std::stoi( state );
    std::cout << "=======================================================" << std::endl;
    std::cout << "Basic CPP: RESTORE STATE called" << std::endl;
    std::cout << "STATE TO BE RESTORED : " << code_state << std::endl;
    std::cout << "=======================================================" << std::endl;
}
// ================================================================================================================
//                                           TIMESTAMP SUBROUTINES
// ================================================================================================================

// =======================================
//             GET TIMESTAMP
// =======================================
void getTimestampCpp(double& timestamp_out, int& status_code, std::string& status_message)
{
    timestamp_out = timestamp;
}

// ================================================================================================================
//                                           INIT SUBROUTINES
// ================================================================================================================

// =======================================
//             INITIALISATION
// =======================================

void initCode(int& status_code, std::string& status_message)
{
    std::string parameters_str;
    initCode(parameters_str, status_code, status_message);
}

// =======================================
//             INITIALISATION
// =======================================

void initCode(std::string parameters_str, int& status_code, std::string& status_message)
{
    IdsNs::IDS::core_profiles core_profiles_in;
    IdsNs::IDS::distribution_sources distribution_sources;
    initCode(core_profiles_in, distribution_sources, parameters_str, status_code, status_message);
}

// =======================================
//        LEGACY-XML INITIALISATION
// =======================================

void initCode(const IdsNs::IDS::core_profiles& core_profiles_in,
                          IdsNs::IDS::distribution_sources& distribution_sources_out,
                          IdsNs::codeparam_t xml_params,
                          int& status_code, std::string& status_message)
{
    std::string parameters_str(*(xml_params.parameters));
    initCode(core_profiles_in, distribution_sources_out, parameters_str, status_code, status_message);
}

// =======================================
//        LEGACY-XML INITIALISATION
// =======================================

void initCode(IdsNs::codeparam_t codeparam, int& status_code, std::string& status_message)
{
    IdsNs::IDS::core_profiles core_profiles_in;
    IdsNs::IDS::distribution_sources distribution_sources;
    initCode(core_profiles_in, distribution_sources, codeparam, status_code, status_message);
}

// =======================================
//             INITIALISATION
// =======================================

void initCode(const IdsNs::IDS::core_profiles& core_profiles_in,
                          IdsNs::IDS::distribution_sources& distribution_sources_out,
                          int& status_code, std::string& status_message)
{
    IdsNs::codeparam_t codeparam;
    initCode(core_profiles_in, distribution_sources_out, codeparam, status_code, status_message);
}

// =======================================
//             INITIALISATION
// =======================================

void initCode(const IdsNs::IDS::core_profiles& core_profiles_in,
                          IdsNs::IDS::distribution_sources& distribution_sources_out,
                          std::string parameters_str,
                          int& status_code, std::string& status_message)
{
    int idsSize = -1;
    status_code = 0;

    status_message = "Basic methods CPP: INIT: OK";

    printf("=======================================================\n");
    printf("Basic methods CPP: INIT starts\n");
    printf("-------------------------------------------------------\n");

    idsSize = core_profiles_in.time.extent(0);
    printf("Size of input IDS  = %d\n", idsSize);

    distribution_sources_out.time.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        distribution_sources_out.time(i) = core_profiles_in.time(i);
    }

    distribution_sources_out.ids_properties.homogeneous_time = 1;
    distribution_sources_out.code.name   = "basic_methods_cpp";
    distribution_sources_out.code.version   = "1.0";

    printf("-------------------------------------------------------\n");
    printf("Basic methods CPP: INIT ends\n");
    printf("=======================================================\n");
}

// ================================================================================================================
//                                           FINALIZE SUBROUTINES
// ================================================================================================================

// =======================================
//             FINALISATION
// =======================================
void cleanUp(int& status_code, std::string& status_message)
{
    IdsNs::IDS::distribution_sources distribution_sources_in;
    IdsNs::IDS::core_profiles core_profiles_out;
    cleanUp(distribution_sources_in, core_profiles_out, status_code, status_message);
}

// =======================================
//             FINALISATION
// =======================================
void cleanUp(const IdsNs::IDS::distribution_sources& distribution_sources_in,
                        IdsNs::IDS::core_profiles& core_profiles_out,
                        std::string parameters_str,
                        int& status_code, std::string& status_message)
{
    int idsSize = -1;

    status_code = 0;
    status_message = "FINALISATION: OK";
    printf("=======================================================\n");
    printf("Basic methods CPP: FINALISATION starts\n");
    printf("-------------------------------------------------------\n");

    idsSize = distribution_sources_in.time.extent(0);
    printf("Size of input IDS  = %d\n", idsSize);

    core_profiles_out.time.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        core_profiles_out.time(i) = distribution_sources_in.time(i);
    }

    core_profiles_out.ids_properties.homogeneous_time = 1;
    core_profiles_out.code.name   = "basic_methods_cpp";
    core_profiles_out.code.version   = "1.0";

    printf("-------------------------------------------------------\n");
    printf("Basic methods CPP: FINALISATION ends\n");
    printf("=======================================================\n");
}

void cleanUp(std::string parameters_str, int& status_code, std::string& status_message)
{
    IdsNs::IDS::distribution_sources distribution_sources_in;
    IdsNs::IDS::core_profiles core_profiles_out;

    cleanUp(distribution_sources_in, core_profiles_out, parameters_str, status_code, status_message);
}

void cleanUp(const IdsNs::IDS::distribution_sources& distribution_sources_in,
                        IdsNs::IDS::core_profiles& core_profiles_out,
                        int& status_code, std::string& status_message)
{
    std::string parameters_str = "";
    cleanUp(distribution_sources_in, core_profiles_out, parameters_str, status_code, status_message);
}

// =======================================
//      LEGACY-XML FINALIZE FUNCTIONS
// =======================================

void cleanUp(IdsNs::codeparam_t codeparam, int& status_code, std::string& status_message)
{
    IdsNs::IDS::distribution_sources distribution_sources_in;
    IdsNs::IDS::core_profiles core_profiles_out;

    cleanUp(distribution_sources_in, core_profiles_out, codeparam, status_code, status_message);
}

void cleanUp(const IdsNs::IDS::distribution_sources& distribution_sources_in,
                        IdsNs::IDS::core_profiles& core_profiles_out,
                        IdsNs::codeparam_t codeparam,
                        int& status_code, std::string& status_message)
{
    std::string parameters_str(*(codeparam.parameters));
    cleanUp(distribution_sources_in, core_profiles_out, parameters_str, status_code, status_message);
}

// ================================================================================================================
//                                           MAIN SUBROUTINES
// ================================================================================================================

// =======================================
//             MAIN
// =======================================
void codeStep(int& status_code, std::string& status_message)
{
    std::string parameters_str;
    codeStep(parameters_str, status_code, status_message);
}

// =======================================
//             MAIN
// =======================================
void codeStep(std::string parameters_str, int& status_code, std::string& status_message)
{
    IdsNs::IDS::equilibrium equilibrium_in;
    IdsNs::IDS::equilibrium equilibrium_out;
    codeStep(equilibrium_in, equilibrium_out, parameters_str, status_code, status_message);
}

// =======================================
//             MAIN
// =======================================
void codeStep(const IdsNs::IDS::equilibrium& equilibrium_in, IdsNs::IDS::equilibrium& equilibrium_out,
                     int& status_code, std::string& status_message)
{
    std::string parameters_str;
    codeStep(equilibrium_in, equilibrium_out, parameters_str, status_code, status_message);
}

// =======================================
//             MAIN
// =======================================
void codeStep(const IdsNs::IDS::equilibrium& equilibrium_in, IdsNs::IDS::equilibrium& equilibrium_out,
                     std::string parameters_str,
                     int& status_code, std::string& status_message)
{
    int idsSize = -1;
    int idsTimeMode = IDS_TIME_MODE_UNKNOWN;

    // INITIALISATION OF ERROR FLAG
    status_code = 0;

    // INITIAL DISPLAY

    printf("=======================================================\n");
    printf("Basic methods CPP: MAIN starts\n");
    printf("-------------------------------------------------------\n");

    // CHECK IF INPUT IDS IS VALID
    idsSize = equilibrium_in.time.extent(0);
    idsTimeMode = equilibrium_in.ids_properties.homogeneous_time;
    if ( idsTimeMode != IDS_TIME_MODE_HOMOGENEOUS && idsSize > 0)
    {
        // ERROR IF THE CODE DOES NOT COMPLETE TO THE END
        status_code = -1;
        status_message = "Error in basic_methods_cpp: input IDS not valid";
        return;
    }

    // READ PARAMETERS
    auto parameters_tuple = extract_values(parameters_str);
    int ntimes = std::get<0>(parameters_tuple);
    float multiplication_factor = std::get<1>(parameters_tuple);

    // MANDATORY FLAG (UNIFORM TIME HERE)
    equilibrium_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;
    equilibrium_out.time.resize(idsSize);
    equilibrium_out.code.output_flag.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        equilibrium_out.time(i) = equilibrium_in.time(i);
        // MODIFY OUTPUT FLAG
        equilibrium_out.code.output_flag(i) = equilibrium_in.time(i) * ntimes * multiplication_factor * code_state;
    }

    // FILL IDS INFO
    equilibrium_out.ids_properties.homogeneous_time = 1;
    equilibrium_out.code.name   = "basic_methods_cpp";
    equilibrium_out.code.version   = "1.0";

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of basic_methods_cpp CPP";
    code_state++;
    timestamp+=10;

    // FINAL DISPLAY
    printf("-------------------------------------------------------\n");
    printf("Basic methods CPP: MAIN ends\n");
    printf("=======================================================\n");
}

// =======================================
//            LEGACY-XML MAIN
// =======================================
void codeStep(IdsNs::codeparam_t xml_params, int& status_code, std::string& status_message)
{
    IdsNs::IDS::equilibrium equilibrium_in;
    IdsNs::IDS::equilibrium equilibrium_out;
    codeStep(equilibrium_in, equilibrium_out, xml_params, status_code, status_message);
}


// =======================================
//            LEGACY-XML MAIN
// =======================================
void codeStep(const IdsNs::IDS::equilibrium& equilibrium_in, IdsNs::IDS::equilibrium& equilibrium_out,
                     IdsNs::codeparam_t xml_params,
                     int& status_code, std::string& status_message)
{
    std::string parameters_str(*(xml_params.parameters));
    codeStep(equilibrium_in, equilibrium_out, parameters_str, status_code, status_message);
}
