
#include <string.h>

{% if code_description.language_specific.mpi.mpi_compiler_cmd %}
#include <mpi.h>
{% endif %}

#include "UALClasses.h"
#include "defs.h"
#include  "{{code_description.language_specific.include_path.split('/')[-1]}}"

{% if code_description.subroutines.init %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  NATIVE INIT SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void init_{{actor_settings.actor_name}}_wrapper(
{% if code_description.code_parameters.parameters   %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info)
{
{% if code_description.code_parameters.parameters %}
	//----  Code parameters ----
    IdsNs::codeparam_t imas_code_params;
    imas_code_params.parameters = (char**)&(code_params->params);
    imas_code_params.default_param = NULL;
    imas_code_params.schema = NULL;

    {% endif %}

        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.subroutines.init}}(
{% if code_description.code_parameters.parameters  %}
            imas_code_params,
{% endif %}
            &(status_info->code), &(status_info->message) );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	if(status_info->code < 0)
		return;
}
{% endif %}

{% if code_description.subroutines.finish %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE FINISH SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
extern "C" void finish_{{actor_settings.actor_name}}_wrapper(
                status_t* status_info)
{


        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.subroutines.finish}}(
            &(status_info->code), &(status_info->message) );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	if(status_info->code < 0)
		return;
}
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   NATIVE MAIN SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

extern "C" void {{actor_settings.actor_name}}_wrapper(
{% for argument in code_description.arguments %}
                ids_description_t* {{ argument.name }}_desc,
{% endfor %}
{% if code_description.code_parameters.parameters %}
                code_parameters_t* code_params,
{% endif %}
                status_t* status_info)
{
{% for argument in code_description.arguments %}
    IdsNs::IDS::{{ argument.type}} {{ argument.name }};
{% endfor %}

{% if code_description.code_parameters.parameters %}
	//----  Code parameters ----
    IdsNs::codeparam_t imas_code_params;
{% endif %}

    IdsNs::IDS *db_entry;

    {% if code_description.language_specific.mpi.mpi_compiler_cmd %}
    //----  MPI  ----
    int mpi_rank;
    int was_mpi_initialized, was_mpi_finalized;

    MPI_Initialized(&was_mpi_initialized);
    if (!was_mpi_initialized)
        MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
{% endif %}


   {% for argument in code_description.arguments %}
    //--------- IDS : {{ argument.name }} ------------------------
    db_entry =   new IdsNs::IDS({{ argument.name }}_desc->idx);
    {{ argument.name }} = db_entry->_{{ argument.type}};
       {% if argument.intent == 'IN' %}
	{{ argument.name }} .get({{ argument.name }}_desc->occurrence);
{% endif %}

    {% endfor %}

        {% if code_description.code_parameters.parameters  %}
    // ------------------ code parameters ----------------------------
    imas_code_params.parameters = (char**)&(code_params->params);
    imas_code_params.default_param = NULL;
    imas_code_params.schema = NULL;
    {% endif %}

        // - - - - - - - - - - - - - NATIVE CODE CALL - - - - - -- - - - - - - - - - - -
    {{code_description.subroutines.main}}(
{% for argument in code_description.arguments %}
            {{ argument.name }},
{% endfor %}
{% if code_description.code_parameters.parameters  %}
            imas_code_params,
{% endif %}
            &(status_info->code), &(status_info->message) );
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	if(status_info->code < 0)
		return;
   // ------------ Provenance information --------------

{% for argument in code_description.arguments if argument.intent == 'OUT' %}
   {{ argument.name }}.code.name = "{{code_description.subroutines.main}}";
   {{ argument.name }}.code.version = "";
{% endfor %}

{% for argument in code_description.arguments if argument.intent == 'OUT' %}
    //--------- PUT IDS : {{ argument.name }} ------------------------
    {{ argument.name }}.put({{ argument.name }}_desc->occurrence);
{% endfor %}


{% if code_description.language_specific.mpi.mpi_compiler_cmd %}
    //----  MPI Finalization ----
    MPI_Finalized(&was_mpi_finalized);
    if (!was_mpi_finalized)
       MPI_Finalize();
{% endif %}

}
