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

{% if code_description.implementation.subroutines.init %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  NATIVE INIT SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_definition("init", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.init, [],
    code_description.implementation.code_parameters.parameters, code_description.settings.mpi_compiler_cmd ) }}
{% endif %}

{% if code_description.implementation.subroutines.finalize %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE FINALIZE SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_definition("finalize", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.finalize, [],
    None, code_description.settings.mpi_compiler_cmd ) }}
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE MAIN SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{{ sbrt_macro.sbrt_definition("main", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.main, code_description.arguments,
code_description.implementation.code_parameters.parameters, code_description.settings.mpi_compiler_cmd ) }}


{% if code_description.implementation.subroutines.get_state %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  NATIVE GET STATUS SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void get_state_{{actor_description.actor_name | lower}}_wrapper(
                char** out_state,
                int* out_status_code, char** out_status_message)
{
    std::string status_msg = "OK";
    std::string state_str = "";


        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
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
//                                  NATIVE SET STATUS SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void set_state_{{actor_description.actor_name | lower}}_wrapper(
                char* state, int* state_str_size,
                int* out_status_code, char** out_status_message)
{
    std::string status_msg = "OK";
    std::string state_str(state);


        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
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
//                                  NATIVE GET STATUS SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void get_timestamp_{{actor_description.actor_name | lower}}_wrapper(
                double* out_timestamp,
                int* out_status_code, char** out_status_message)
{
    std::string status_msg = "OK";

        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.implementation.subroutines.get_timestamp}}(*out_timestamp, *out_status_code, status_msg );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // converting status info
    convert_status_info(status_msg, out_status_message);

	if(*out_status_code < 0)
		return;
}
{% endif %}
