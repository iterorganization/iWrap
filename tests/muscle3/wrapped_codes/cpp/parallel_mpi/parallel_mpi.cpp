#include <mpi.h>
#include "parallel_mpi.h"

int code_state = 0;

// =======================================
//             INITIALISATION
//=======================================

void init_code (int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;

    status_code = 0;
    status_message = "INITIALISATION: OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: INITIALISATION called <" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             FINALISATION
//=======================================
void clean_up( int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;

    status_code = 0;
    status_message = "FINALISATION: OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: FINALISATION called <" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             MAIN
//=======================================
void code_step(const IdsNs::IDS::core_profiles& in_core_profiles,
                         IdsNs::IDS::distribution_sources& out_distribution_sources,
                         int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    int idsSize = -1;

    status_code = 0;
    status_message = "STEP OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout <<  "=======================================" << std::endl;
    std::cout <<  "START OF PHYSICS CODE<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout <<  "Starting from: " << code_state << std::endl;

    for (int i = 0; i < 20; i++)
    {
        // ANY COMPUTATIONS
        code_state++;
    }

    std::cout <<  "Counting to : " << code_state << std::endl;

    idsSize = in_core_profiles.time.extent(0);
    std::cout <<  "Size of input IDS: " <<idsSize << " <"<< mpi_rank << "/" << mpi_size << ">" << std::endl;

    if (idsSize > 0) {
        out_distribution_sources.time.resize(idsSize);
            // Fill in the output IDS (Physical data)
        for(int i=0; i < idsSize; i++)
        {
            // Time : copy from input IDS
            out_distribution_sources.time(i) =  1000000 * mpi_rank + 100 * code_state + in_core_profiles.time(i);
        }
    }
    else {
            out_distribution_sources.time.resize(1);
            out_distribution_sources.time(1) = 1000000 * mpi_rank + 100 * code_state;
    }
    out_distribution_sources.ids_properties.homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS;
    out_distribution_sources.code.name   = "cp2ds_mpi_cpp";
    out_distribution_sources.code.version   = "1.0";
    out_distribution_sources.code.output_flag = 0   ;


    std::cout <<  "END OF PHYSICS CODE" << std::endl;
    std::cout <<  "=======================================" << std::endl;

}

// =======================================
//             GET STATE
//=======================================


void get_code_state( std::string& state_out, int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    status_code = 0;

    status_message = "INITIALISATION: OK";
    state_out = std::to_string(code_state);

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: GET STATE called<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "STATE is : " << state_out << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             SET STATE
//=======================================
void restore_code_state( std::string state, int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    status_code = 0;
    status_message = "FINALISATION: OK";

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    code_state = std::stoi( state );
    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: RESTORE STATE called<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "STATE TO BE RESTORED : " << code_state << std::endl;
    std::cout << "=======================================================" << std::endl;
}

// =======================================
//             GET TIMESTAMP
//=======================================
void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message)
{
    int mpi_size, mpi_rank;
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    timestamp_out = (double) code_state;

    std::cout << "=======================================================" << std::endl;
    std::cout << "CPP MPI: GET TIMESTAMP called<" << mpi_rank << "/" << mpi_size << ">" << std::endl;
    std::cout << "TIMESTAMP : " << timestamp_out << std::endl;
    std::cout << "=======================================================" << std::endl;
}