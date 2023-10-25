#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

#include "code_restart.h"

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
//             MAIN
//=======================================
void code_restart(const IdsNs::IDS::equilibrium& in_equilibrium,
                     int& status_code, std::string& status_message)
{

    // INITIALISATION OF ERROR FLAG
    status_code = 0;
    
    // INITIAL DISPLAY
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  "START OF PHYSICS CODE" << std::endl;
    

    std::cout <<  "Starting from: " << code_state << std::endl;



    for (int i = 0; i < 20; i++)
    {
        // COMPUTATIONS
        code_state++;
    }


    std::cout <<  "Counting to : " << code_state << std::endl;

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of code_restart CPP";

    
    // FINAL DISPLAY
    std::cout <<  "END OF PHYSICS CODE" << std::endl;
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  " " << std::endl;
}
