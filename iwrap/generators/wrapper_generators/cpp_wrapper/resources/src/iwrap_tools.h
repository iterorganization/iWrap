#ifndef _IWRAP_TOOLS
#define _IWRAP_TOOLS

#include "defs.h"

{% if build_info.al_version.startswith('4.')   %}
    #include "UALClasses.h"
{% else %}
    #include "ALClasses.h"
{% endif %}

char* iwrap_trim(char* text, int text_size);

int read_input(const char* file_name, ids_description_t db_entry_desc_array[], int array_expected_size);

void read_code_parameters( char** xml_string);

int write_output(const char* file_name, int status_code, char* status_message);

void handle_status_info(int status_code, char* status_message, const char* actor_name, const char* method_name);


void release_status_info(char* status_message);

void convert_status_info(std::string in_status_msg, char** out_status_msg);


IdsNs::IDS* init_db(ids_description_t* db_entry_desc);

int open_db(IdsNs::IDS* db_entry, ids_description_t* db_entry_desc);

void close_db(IdsNs::IDS* db_entry);

#endif // _IWRAP_TOOLS
