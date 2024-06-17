#include <string>
#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

#include "level2_cpp_json.h"

void physics_ii_cpp(const IdsNs::IDS::equilibrium& in_equilibrium, IdsNs::IDS::equilibrium& out_equilibrium,
                    std::string codeparam,
                    int& status_code, std::string& status_message)
{

    // ---------------------------------------
    // PURPOSE: SIMPLE PSEUDO PHYSICS CODE 
    // (IT READS AN EQUILIBRIUM IDS,
    // MODIFIES ONE VARIABLE IN THE IDS, 
    // AND WRITES THE NEW EQUILIBRIUM IDS)
    // ---------------------------------------
    const char* ERROR_MESSAGE = "Error in physics_ii: input IDS not valid";
    
    int idsSize = -1;
    int idsTimeMode = IDS_TIME_MODE_UNKNOWN;
    
    // INITIALISATION OF ERROR FLAG
    status_code = 0;
    
    // INITIAL DISPLAY
    printf( " \n");
    printf( "=======================================\n");
    printf( "START OF PHYSICS CODE\n");
    
    // CHECK IF INPUT IDS IS VALID
    idsSize = in_equilibrium.time.extent(0);
    idsTimeMode = in_equilibrium.ids_properties.homogeneous_time;
    if ( idsTimeMode != IDS_TIME_MODE_HOMOGENEOUS && idsSize > 0)
    {   
        // ERROR IF THE CODE DOES NOT COMPLETE TO THE END
        status_code = -1;
        status_message = ERROR_MESSAGE;
        return;
    }

    const auto rawJsonLength = static_cast<int>(codeparam.length());
    JSONCPP_STRING err;
    Json::Value root;

    Json::CharReaderBuilder builder;
    const std::unique_ptr<Json::CharReader> reader(builder.newCharReader());
    if (!reader->parse(codeparam.c_str(), codeparam.c_str() + rawJsonLength, &root, &err))
    {
      std::cout << "error reading JSON file" << std::endl;
      return;
    }

    const int ntimes = root["parameters"]["ntimes"].asInt();
    const float multiplication_factor = root["parameters"]["multiplication_factor"].asFloat();


    printf( "------------------------------------\n");
    printf( "Parameters read from input JSON file:\n");
    printf(" ntimes                = %d\n",  ntimes);
    printf(" multiplication_factor = %f\n",  multiplication_factor);

    printf( "------------------------------------\n");
    
    // COPY THE INPUT IDS IN THE OUTPUT IDS
    out_equilibrium = in_equilibrium;
    
    // INITIAL PLASMA MAJOR RADIUS
    printf("Initial plasma major radius = %f\n", in_equilibrium.time_slice(0).boundary.geometric_axis.r);
    
    // MODIFY PLASMA MAJOR RADIUS
    out_equilibrium.time_slice.resize(1);
    double r = in_equilibrium.time_slice(0).boundary.geometric_axis.r;
    r = r * 0.5;
    out_equilibrium.time_slice(0).boundary.geometric_axis.r = r;
    
    // MANDATORY FLAG (UNIFORM TIME HERE)
    out_equilibrium.ids_properties.homogeneous_time = 1;
    out_equilibrium.time.resize(idsSize);
    out_equilibrium.time = in_equilibrium.time;

    // FINAL PLASMA MAJOR RADIUS
    printf(" Final plasma major radius   = %f\n",  out_equilibrium.time_slice(0).boundary.geometric_axis.r);
    
    
    // FINAL DISPLAY
    printf( "END OF PHYSICS CODE\n");
    printf( "=======================================\n");
    printf( " \n");
}
