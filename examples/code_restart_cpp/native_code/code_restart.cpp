#include "UALClasses.h"

#include "code_restart.h"

// =======================================
//             GET STATE
//=======================================

void get_code_state( std::string& state_out, int& status_code, std::string& status_message)
{
    status_code = 0;

   status_message = "INITIALISATION: OK";
   state_out = "<Representation of code internal state>";

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

    std::cout << "=======================================================" << std::endl;
    std::cout << "Code lifecycle CPP: RESTORE STATE called" << std::endl;
    std::cout << "STATE TO BE RESTORED : " << state << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             MAIN
//=======================================
void code_restart(const IdsNs::IDS::equilibrium& in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium,
                     IdsNs::codeparam_t xml_params,
                     int& status_code, std::string& status_message)
{
    int idsSize = -1;
    int idsTimeMode = IDS_TIME_MODE_UNKNOWN;
    
    // INITIALISATION OF ERROR FLAG
    status_code = 0;
    
    // INITIAL DISPLAY
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  "START OF PHYSICS CODE" << std::endl;
    
    // CHECK IF INPUT IDS IS VALID
    idsSize = in_equilibrium.time.extent(0);
    idsTimeMode = in_equilibrium.ids_properties.homogeneous_time;
    if ( idsTimeMode != IDS_TIME_MODE_HOMOGENEOUS && idsSize > 0)
    {   
        // ERROR IF THE CODE DOES NOT COMPLETE TO THE END
        status_code = -1;
        status_message = "Error in code_lifecycle: input IDS not valid";
        return;
    }
    
    // MANDATORY FLAG (UNIFORM TIME HERE)
    out_equilibrium.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;
    out_equilibrium.time.resize(idsSize);

    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        out_equilibrium.time(i) = in_equilibrium.time(i);
    }

    out_equilibrium.ids_properties.homogeneous_time = 1;
    out_equilibrium.code.name   = "code_restart_cpp";
    out_equilibrium.code.version   = "1.0";
    out_equilibrium.code.parameters = "my_code_specific_parameters";
    out_equilibrium.code.output_flag.resize(1);
    out_equilibrium.code.output_flag(0) = 0;

    std::cout << "Size of input IDS  = " << idsSize << std::endl;

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of code_lifecycle CPP";

    
    // FINAL DISPLAY
    std::cout <<  "END OF PHYSICS CODE" << std::endl;
    std::cout <<  "=======================================" << std::endl;
    std::cout <<  " " << std::endl;
}
