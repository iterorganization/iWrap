
#include "iwrap_tools.h"
#include "cpp_wrapper.h"

int main(int argc, char **argv) 
{
   const int IDS_ARGS_NO = {{code_description.arguments | length}};
   ids_description_t db_entry_desc_array[IDS_ARGS_NO];
   IdsNs::IDS* *db_entry_array ;

    //----  Status info  ----
    status_t status_info;

{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
    //----  Code parameters  ----
    const char* PARAM_DIR = "../input/";
    const char* XML_FILE = "{{code_description.code_parameters.parameters.split('/')[-1]}}";
    const char* XSD_FILE = "{{code_description.code_parameters.schema.split('/')[-1]}}";
    code_parameters_t code_params;

    code_params = read_codeparams(PARAM_DIR, XML_FILE, XSD_FILE);
{% endif %}

    read_input(db_entry_desc_array, IDS_ARGS_NO);

    db_entry_array = open_db_entries(db_entry_desc_array, IDS_ARGS_NO);

    //!!!!!!!!! Cpp wrapper !!!!!!!!!!!!!!!
    {{code_description.code_name}}_wrapper(
{% for argument in code_description.arguments %}
                &db_entry_desc_array[{{loop.index - 1 }}],
{% endfor %}
{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
                &code_params,
{% endif %}
                &status_info);
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    handle_status_info(status_info, "{{actor_settings.actor_name}}");

    //-----------Writing output data to file ---------------------
    write_output(status_info);

    close_db_entries(db_entry_array, IDS_ARGS_NO);
}
