
#include <string.h>

#include "UALClasses.h"
#include "defs.h"
#include  "{{code_description.language_specific.include_path.split('/')[-1]}}"


extern "C" void {{code_description.code_name}}_wrapper(
{% for argument in code_description.arguments %}
                ids_description_t* {{ argument.name }}_desc,
{% endfor %}
{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info)
{
{% for argument in code_description.arguments %}
    IdsNs::IDS::{{ argument.type}} {{ argument.name }};
{% endfor %}

{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
	//----  Code parameters ----
    IdsNs::codeparam_t imas_code_params;
{% endif %}




    IdsNs::IDS *db_entry;

   {% for argument in code_description.arguments %}
    //--------- IDS : {{ argument.name }} ------------------------
    db_entry =   new IdsNs::IDS({{ argument.name }}_desc->idx);
    {{ argument.name }} = db_entry->_{{ argument.type}};
       {% if argument.intent == 'IN' %}
	{{ argument.name }} .get({{ argument.name }}_desc->occurrence);
{% endif %}

    {% endfor %}

        {% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
    // ------------------ code parameters ----------------------------
    //imas_code_params = convert(code_params)
    (imas_code_params.parameters) = (char**)&(code_params->params);
    (imas_code_params.default_param) = (char**)&(code_params->params);
    (imas_code_params.schema) = (char**)&(code_params->schema);

    {% endif %}

        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.code_name}}(
{% for argument in code_description.arguments %}
            {{ argument.name }},
{% endfor %}
{% if code_description.code_parameters.parameters and code_description.code_parameters.schema %}
            imas_code_params,
{% endif %}
            &(status_info->code), &(status_info->message) );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	if(status_info->code < 0)
		return;
   // ------------ Provenance information --------------

{% for argument in code_description.arguments if argument.intent == 'OUT' %}
   {{ argument.name }}.code.name = "{{code_description.code_name}}";
   {{ argument.name }}.code.version = "";
{% endfor %}

{% for argument in code_description.arguments if argument.intent == 'OUT' %}
    //--------- PUT IDS : {{ argument.name }} ------------------------
    {{ argument.name }}.put({{ argument.name }}_desc->occurrence);
{% endfor %}

	
	


}
