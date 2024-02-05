{% import './macros/%s_ids.jinja2' % code_description.implementation.data_type as ids_macro %}
{% import './macros/subroutines.jinja2' as sbrt_macro %}
#ifndef _CPP_WRAPPER
#define _CPP_WRAPPER

{{ ids_macro.imports(build_info.al_version) }}
#include "defs.h"

{% if code_description.implementation.subroutines.init.name %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  NATIVE INIT SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_declaration("init", actor_description.actor_name,
                                    code_description.implementation.subroutines.init,
                                    code_description.implementation.code_parameters.parameters ) }}
{% endif %}

{% if code_description.implementation.subroutines.finalize.name %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE FINISH SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_declaration("finalize", actor_description.actor_name,
                                    code_description.implementation.subroutines.finalize,
                                     code_description.implementation.code_parameters.parameters ) }}
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE MAIN SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_declaration("main", actor_description.actor_name,
                                    code_description.implementation.subroutines.main,
                                    code_description.implementation.code_parameters.parameters ) }}

{% if code_description.implementation.subroutines.get_state %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE GET STATE SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_description.actor_name | lower}}_wrapper_get_state(
                char** status_out,
                int* out_status_code, char** out_status_message);
{% endif %}

{% if code_description.implementation.subroutines.set_state %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE SET STATE SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_description.actor_name | lower}}_wrapper_set_state(
                char* state, int* state_str_size,
                int* out_status_code, char** out_status_message);
{% endif %}

{% if code_description.implementation.subroutines.get_timestamp %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE GET TIMESTAMP SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_description.actor_name | lower}}_wrapper_get_timestamp(
                double* timestamp_out,
                int* out_status_code, char** out_status_message);
{% endif %}

#endif // _CPP_WRAPPER
