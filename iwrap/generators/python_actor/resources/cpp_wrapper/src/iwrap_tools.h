#ifndef _IWRAP_TOOLS
#define _IWRAP_TOOLS

#include "UALClasses.h"
#include "defs.h"


int read_input(ids_description_t db_entry_desc_array[], int array_expected_size);

int write_output(status_t status_info);

int handle_status_info(status_t status_info, const char* actor_name);

code_parameters_t read_codeparams(const char* param_dir, const char* xml_file, const char* xsd_file);

IdsNs::IDS** open_db_entries(ids_description_t* db_entry_desc_array, int array_size);

int close_db_entries(IdsNs::IDS** db_entry_array, int array_size);

#endif // _IWRAP_TOOLS