#ifndef _CPP_WRAPPER
#define _CPP_WRAPPER

#include "UALClasses.h"
#include "defs.h"



extern "C" void {{actor_description.actor_name}}_wrapper(
{% for argument in code_description.arguments %}
                ids_description_t* {{ argument.name }}_desc,
{% endfor %}
{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info);

#endif // _CPP_WRAPPER