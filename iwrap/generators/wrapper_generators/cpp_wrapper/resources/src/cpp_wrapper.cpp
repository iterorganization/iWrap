{% import './macros/%s_ids.jinja2' % code_description.implementation.data_type as ids_macro %}
{% import './macros/subroutines.jinja2' as sbrt_macro %}

#include <string>


{% if code_description.settings.mpi_compiler_cmd %}
#include <mpi.h>
{% endif %}

{{ ids_macro.imports(build_info.al_version) }}
#include "defs.h"
#include "iwrap_tools.h"
#include  "{{code_description.implementation.include_path | basename }}"

{% if code_description.implementation.subroutines.init.name %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  INIT SBRT WRAPPER
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_definition("init", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.init,
    code_description.implementation.code_parameters, code_description.settings.mpi_compiler_cmd ) }}
{% endif %}

{% if code_description.implementation.subroutines.finalize.name %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   FINALIZE SBRT WRAPPER
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_definition("finalize", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.finalize,
    code_description.implementation.code_parameters, code_description.settings.mpi_compiler_cmd ) }}
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   MAIN SBRT WRAPPER
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{{ sbrt_macro.sbrt_definition("main", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.main,
code_description.implementation.code_parameters, code_description.settings.mpi_compiler_cmd ) }}


{% if code_description.implementation.subroutines.get_state %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   GET_STATE SBRT WRAPPER
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_description.actor_name | lower}}_wrapper_get_state(
                char** out_state,
                int* out_status_code, char** out_status_message)
{
    std::string status_msg = "OK";
    std::string state_str = "";


    // - - - - - - - - - - - - - - CODE SBRT CALL - - - - - - - - - - - - - - - - - -
    {{code_description.implementation.subroutines.get_state}}(state_str, *out_status_code, status_msg );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // converting status info
    convert_status_info(status_msg, out_status_message);
    convert_status_info(state_str, out_state);

	if(*out_status_code < 0)
		return;
}
{% endif %}

{% if code_description.implementation.subroutines.set_state %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   SET_STATE SBRT WRAPPER
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_description.actor_name | lower}}_wrapper_set_state(
                char* state, int* state_str_size,
                int* out_status_code, char** out_status_message)
{
    std::string status_msg = "OK";
    std::string state_str(state);


    // - - - - - - - - - - - - - CODE SBRT CALL - - - - - - - - - - - - - - - - - -
    {{code_description.implementation.subroutines.set_state}}(state_str, *out_status_code, status_msg );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // converting status info
    convert_status_info(status_msg, out_status_message);

	if(*out_status_code < 0)
		return;
}
{% endif %}

{% if code_description.implementation.subroutines.get_timestamp %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  GET_TIMESTAMP SBRT WRAPPER
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_description.actor_name | lower}}_wrapper_get_timestamp(
                double* out_timestamp,
                int* out_status_code, char** out_status_message)
{
    std::string status_msg = "OK";

        // - - - - - - - - - - - - - CODE SBRT CALL - - - - - - - - - - - - - - - - - -
    {{code_description.implementation.subroutines.get_timestamp}}(*out_timestamp, *out_status_code, status_msg );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // converting status info
    convert_status_info(status_msg, out_status_message);

	if(*out_status_code < 0)
		return;
}
{% endif %}
