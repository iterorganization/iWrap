#ifndef _CPP_WRAPPER
#define _CPP_WRAPPER

#include "UALClasses.h"
#include "defs.h"

{% if code_description.subroutines.init %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  NATIVE INIT SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void init_{{actor_settings.actor_name}}_wrapper(
{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info);

{% endif %}

{% if code_description.subroutines.finish %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE FINISH SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void finish_{{actor_settings.actor_name}}_wrapper(status_t* status_info);
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE MAIN SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void {{actor_settings.actor_name}}_wrapper(
{% for argument in code_description.arguments %}
                ids_description_t* {{ argument.name }}_desc,
{% endfor %}
{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info);

#endif // _CPP_WRAPPER