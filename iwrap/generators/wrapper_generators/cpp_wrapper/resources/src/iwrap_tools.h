#ifndef _IWRAP_TOOLS
#define _IWRAP_TOOLS

#include "UALClasses.h"
#include "defs.h"

int read_input(ids_description_t db_entry_desc_array[], int array_expected_size);

int read_code_parameters(char** xml_string);

int write_output(int status_code, char* status_message);

int handle_status_info(int status_code, char* status_message, const char* actor_name);

void release_status_info(char* status_message);

int convert_status_info(std::string in_status_msg, char** out_status_msg);

IdsNs::IDS** open_db_entries(ids_description_t* db_entry_desc_array, int array_size);

void close_db_entries(IdsNs::IDS** db_entry_array, int array_size);

#endif // _IWRAP_TOOLS