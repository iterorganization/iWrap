#if AL_MAJOR == 5
    #include "ALClasses.h"
#elif AL_MAJOR == 4
    #include "UALClasses.h"
#else
    #warning Could not find AL_MAJOR variable. Assuming AL version = 5.x.x.
    #include "ALClasses.h"
#endif

#include "parallel_mpi.h"
#include <mpi.h>

void codeStep(const IdsNs::IDS::core_profiles& core_profiles_in,
                         IdsNs::IDS::distribution_sources& distribution_sources_out,
                         int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    int idsSize = -1;


    printf("Entering subroutine parallel_mpi::codeStep()\n");
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    int world_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    printf("INFO: Process <%d>, out of: %d\n", mpi_rank, mpi_size);


    status_code = 0;

    idsSize = core_profiles_in.time.extent(0);

    printf("Size of input IDS  = %d\n", idsSize);

    // INITIALISATION OF STATUS INFO
    status_message = "Status info of parallel_mpi::codeStep() CPP";

    distribution_sources_out.time.resize(idsSize);



    // Fill in the output IDS (Physical data)
    for(int i=0; i < idsSize; i++)
    {
        // Time : copy from input IDS
        distribution_sources_out.time(i) = 10000 * mpi_rank + core_profiles_in.time(i);
    }

    distribution_sources_out.ids_properties.homogeneous_time = 1;
    distribution_sources_out.code.name   = "parallel_mpi CPP";
    distribution_sources_out.code.version   = "1.0";
    distribution_sources_out.code.parameters = "my_code_specific_parameters";
    distribution_sources_out.code.output_flag = 0   ;


    printf("End of parallel_mpi::codeStep() CPP\n");

}
