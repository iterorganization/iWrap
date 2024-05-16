#include "ALClasses.h"
#include "code2.h"

std::string farewell;

//===================================================================
//                            MAIN
//===================================================================
void code2_step(const IdsNs::IDS::distribution_sources ids_in,
                    IdsNs::IDS::core_profiles& ids_out,
                    int& status_code, std::string& status_message)
{
    printf("  Code CPP: MAIN\n");

    // Code specific initialization actions
    farewell = " C++:  Goodbye!";

    // Computations specific to the code
    ids_out.ids_properties.homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS;  // Mandatory field
    ids_out.ids_properties.comment = ids_in.ids_properties.comment + farewell + " | ";


    // Code specific finalization actions
    farewell.clear();

    // Setting status flag and message
    status_code = 0;
    status_message = "OK";
}

