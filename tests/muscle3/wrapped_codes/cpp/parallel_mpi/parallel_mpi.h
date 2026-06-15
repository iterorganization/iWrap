#ifndef _PARALLEL_MPI_CPP
#define _PARALLEL_MPI_CPP

#include "ALClasses.h"

void init_code (int& status_code, std::string& status_message);

void clean_up( int& status_code, std::string& status_message);

void code_step(const IdsNs::IDS::core_profiles& core_profiles_in,
                    IdsNs::IDS::distribution_sources& distribution_sources_out,
                    int& status_code, std::string& status_message);

void get_code_state( std::string& state_out, int& status_code, std::string& status_message);

void restore_code_state( std::string state, int& status_code, std::string& status_message);

void get_timestamp_cpp(double& timestamp_out, int& status_code, std::string& status_message);

#endif // _PARALLEL_MPI_CPP