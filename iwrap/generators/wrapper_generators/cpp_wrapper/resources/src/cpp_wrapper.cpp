{% import './macros/%s_ids.jinja2' % code_description.implementation.data_type as ids_macro %}
#include <string.h>


{% if code_description.settings.mpi_compiler_cmd %}
#include <mpi.h>
{% endif %}

{{ ids_macro.imports() }}
#include "defs.h"
#include  "{{code_description.implementation.include_path.split('/')[-1]}}"

{% if code_description.implementation.subroutines.init %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  NATIVE INIT SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void init_{{actor_description.actor_name}}_wrapper(
{% if code_description.implementation.code_parameters.parameters   %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info)
{
{% if code_description.implementation.code_parameters.parameters %}
	//----  Code parameters ----
    IdsNs::codeparam_t imas_code_params;
    imas_code_params.parameters = (char**)&(code_params->params);
    imas_code_params.default_param = NULL;
    imas_code_params.schema = NULL;

    {% endif %}

        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.implementation.subroutines.init}}(
{% if code_description.implementation.code_parameters.parameters  %}
            imas_code_params,
{% endif %}
            &(status_info->code), &(status_info->message) );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	if(status_info->code < 0)
		return;
}
{% endif %}

{% if code_description.implementation.subroutines.finish %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE FINISH SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void finish_{{actor_description.actor_name}}_wrapper(
                status_t* status_info)
{


        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.implementation.subroutines.finish}}(
            &(status_info->code), &(status_info->message) );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	if(status_info->code < 0)
		return;
}
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE MAIN SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

extern "C" void {{actor_description.actor_name}}_wrapper(
{% for argument in code_description.arguments %}
                ids_description_t* {{ argument.name }}_desc,
{% endfor %}
{% if code_description.implementation.code_parameters.parameters %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info)
{

{% for argument in code_description.arguments %}
    // IDS : {{ argument.name }} ------------------------
    {{ ids_macro.declare(argument.type, argument.name ) }}
{% endfor %}

{% if code_description.implementation.code_parameters.parameters %}
	//----  Code parameters ----
    IdsNs::codeparam_t imas_code_params;
{% endif %}


    {% if code_description.settings.mpi_compiler_cmd %}
    //----  MPI  ----
    int mpi_rank;
    int was_mpi_initialized, was_mpi_finalized;

    MPI_Initialized(&was_mpi_initialized);
    if (!was_mpi_initialized)
        MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
{% endif %}


    {% for argument in code_description.arguments if argument.intent == 'IN' %}
    //--------- Get IDS : {{ argument.name }} ------------------------
    {{ ids_macro.get( argument.name) }}
    {% endfor %}

        {% if code_description.implementation.code_parameters.parameters  %}
    // ------------------ code parameters ----------------------------
    imas_code_params.parameters = (char**)&(code_params->params);
    imas_code_params.default_param = NULL;
    imas_code_params.schema = NULL;
    {% endif %}

        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.implementation.subroutines.main}}(
{% for argument in code_description.arguments %}
            {{ argument.name }},
{% endfor %}
{% if code_description.implementation.code_parameters.parameters  %}
            imas_code_params,
{% endif %}
            &(status_info->code), &(status_info->message) );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	if(status_info->code < 0)
		return;
   // ------------ Provenance information --------------
{% for argument in code_description.arguments if argument.intent == 'OUT' %}
    {{ ids_macro.provenance( argument.name , code_description.implementation.subroutines.main) }}
{% endfor %}

{% for argument in code_description.arguments if argument.intent == 'OUT' %}
    //--------- PUT IDS : {{ argument.name }} ------------------------
    {{ ids_macro.put( argument.name) }}
{% endfor %}

{% for argument in code_description.arguments %}
    //--------- PUT IDS : {{ argument.name }} ------------------------
    {{ ids_macro.deallocate( argument.name) }}
{% endfor %}


{% if code_description.settings.mpi_compiler_cmd %}
    //----  MPI Finalization ----
    MPI_Finalized(&was_mpi_finalized);
    if (!was_mpi_finalized)
       MPI_Finalize();
{% endif %}

}
