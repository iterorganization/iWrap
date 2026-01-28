#include <unistd.h>

#include "basic.h"

// =======================================
//             GET STATE
//=======================================

int code_state = 0;

void get_code_state( std::string& state_out, int& status_code, std::string& status_message)
{
    status_code = 0;

    status_message = "INITIALISATION: OK";
    state_out = std::to_string(code_state);

    std::cout << "=======================================================" << std::endl;
    std::cout << "Code restart CPP: GET STATE called" << std::endl;
    std::cout << "STATE is : " << state_out << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             SET STATE
//=======================================
void restore_code_state( std::string state, int& status_code, std::string& status_message)
{
    status_code = 0;
    status_message = "FINALISATION: OK";

    code_state = std::stoi( state );
    std::cout << "=======================================================" << std::endl;
    std::cout << "Code lifecycle CPP: RESTORE STATE called" << std::endl;
    std::cout << "STATE TO BE RESTORED : " << code_state << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             GET TIMESTAMP
//=======================================
void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message)
{
    timestamp_out = (double) code_state;
}

// =======================================
//             INITIALISATION
//=======================================

void init_code (int& status_code, std::string& status_message)
{
    status_code = 0;

   status_message = "INITIALISATION: OK";

    printf("=======================================================\n");
    printf("Code lifecycle CPP: INITIALISATION called\n");
    printf("=======================================================\n");
    //    printf( "%s\n", *(codeparam.parameters));
    printf( "\n=======================================\n");
}

// =======================================
//             FINALISATION
//=======================================
void clean_up( int& status_code, std::string& status_message)
{
    status_code = 0;
    status_message = "FINALISATION: OK";

    printf("=======================================================\n");
    printf("Code lifecycle CPP: FINALISATION called\n");
    printf("=======================================================\n");
}

// =======================================
//             MAIN
//=======================================
void code_step(const IdsNs::IDS::core_profiles& core_profiles_in,
                    IdsNs::IDS::distribution_sources& distribution_sources_out,
                    int& status_code, std::string& status_message)
{
    int idsSize = 0;

    // INITIALISATION OF ERROR FLAG
    status_code = 0;
    
    // INITIAL DISPLAY
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  "START OF PHYSICS CODE" << std::endl;
    

    std::cout <<  "Starting from: " << code_state << std::endl;
    std::cout <<  "PWD: " << get_current_dir_name() << std::endl;



    for (int i = 0; i < 20; i++)
    {
        // COMPUTATIONS
        code_state++;
    }


    std::cout <<  "Counting to : " << code_state << std::endl;

    distribution_sources_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;
    idsSize = core_profiles_in.time.extent(0);
    std::cout <<  "Size of input IDS: " <<idsSize << std::endl;

    if (idsSize > 0) {
        distribution_sources_out.time.resize(idsSize);
            // Fill in the output IDS (Physical data)
        for(int i=0; i < idsSize; i++)
        {
            // Time : copy from input IDS
            distribution_sources_out.time(i) =  1000 * code_state + core_profiles_in.time(i);
        }
    }
    else {
            distribution_sources_out.time.resize(1);
            distribution_sources_out.time(1) = 1000 * code_state;
    }

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of code_restart CPP";

    
    // FINAL DISPLAY
    std::cout <<  "END OF PHYSICS CODE" << std::endl;
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  " " << std::endl;
}
