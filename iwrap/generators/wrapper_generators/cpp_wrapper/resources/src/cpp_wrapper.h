{% import './macros/%s_ids.jinja2' % code_description.implementation.data_type as ids_macro %}
#ifndef _CPP_WRAPPER
#define _CPP_WRAPPER

{{ ids_macro.imports() }}
#include "defs.h"

{% if code_description.implementation.subroutines.init %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  NATIVE INIT SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void init_{{actor_description.actor_name | lower}}_wrapper(
{% if code_description.implementation.code_parameters.parameters and code_description.implementation.code_parameters.schema %}
                char* code_params_str,
{% endif %}
                int* out_status_code, char** out_status_message);

{% endif %}

{% if code_description.implementation.subroutines.finalize %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE FINISH SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void finish_{{actor_description.actor_name | lower}}_wrapper(int* out_status_code, char** out_status_message);
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE MAIN SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_description.actor_name | lower}}_wrapper(
{% for argument in code_description.arguments %}
                ids_description_t* {{ argument.name }}_desc,
{% endfor %}
{% if code_description.implementation.code_parameters.parameters and code_description.implementation.code_parameters.schema %}
                char* code_params_str,
{% endif %}
                int* out_status_code, char** out_status_message);

{% if code_description.implementation.subroutines.get_state %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE GET STATE SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void get_state_{{actor_description.actor_name | lower}}_wrapper(
                char** status_out,
                int* out_status_code, char** out_status_message);
{% endif %}

{% if code_description.implementation.subroutines.set_state %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE SET STATE SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void set_state_{{actor_description.actor_name | lower}}_wrapper(
                char* state, int* state_str_size,
                int* out_status_code, char** out_status_message);
{% endif %}

{% if code_description.implementation.subroutines.get_timestamp %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE GET TIMESTAMP SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void get_timestamp_{{actor_description.actor_name | lower}}_wrapper(
                double* timestamp_out,
                int* out_status_code, char** out_status_message);
{% endif %}

#endif // _CPP_WRAPPER