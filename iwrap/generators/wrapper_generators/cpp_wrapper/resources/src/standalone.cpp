{% if code_description.settings.mpi_compiler_cmd %}

#include <mpi.h>
{% endif %}
#include "iwrap_tools.h"
#include "cpp_wrapper.h"

int main(int argc, char **argv) 
{
   const int IDS_ARGS_NO = {{code_description.arguments | length}};
   ids_description_t db_entry_desc_array[IDS_ARGS_NO];
   IdsNs::IDS* *db_entry_array ;

    //----  Status info  ----
    status_t status_info;

{% if code_description.settings.mpi_compiler_cmd %}
    //----  MPI  ----
    int mpi_rank;
    int was_mpi_finalized, was_mpi_initialized;

    MPI_Initialized(&was_mpi_initialized);
    if (!was_mpi_initialized)
        MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
{% endif %}

  //----  Code parameters  ----
      char *xml_string;

  read_input(db_entry_desc_array, IDS_ARGS_NO, &xml_string);

{% if code_description.implementation.code_parameters.parameters %}
    code_parameters_t code_params;

    code_params.params = xml_string;
    code_params.params_size = strlen(xml_string);
{% endif %}


     {% if code_description.implementation.subroutines.init %}
    // - - - - - - - - - - - - - - - - - -INIT SBRT CALL - - - - - - - - - - - - - - - - - - - - - - - - - -
    init_{{actor_description.actor_name | lower}}_wrapper(
        {% if code_description.implementation.code_parameters.parameters and code_description.implementation.code_parameters.schema %}
                &code_params,
        {% endif %}
                &status_info);
    {% endif %}



    db_entry_array = open_db_entries(db_entry_desc_array, IDS_ARGS_NO);

    //!!!!!!!!! Cpp wrapper !!!!!!!!!!!!!!!
    {{actor_description.actor_name | lower}}_wrapper(
{% for argument in code_description.arguments %}
                &db_entry_desc_array[{{loop.index - 1 }}],
{% endfor %}
{% if code_description.implementation.code_parameters.parameters and code_description.implementation.code_parameters.schema %}
                &code_params,
{% endif %}
                &status_info);
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   {% if code_description.settings.mpi_compiler_cmd %}
    if (mpi_rank == 0)
    {
       // --- called only for RANK 0 process
{% endif %}

    handle_status_info(status_info, "{{actor_description.actor_name}}");

    //-----------Writing output data to file ---------------------
    write_output(status_info);


{% if code_description.settings.mpi_compiler_cmd %}
    } //The end of section called only for RANK 0 process
{% endif %}
    close_db_entries(db_entry_array, IDS_ARGS_NO);


     {% if code_description.implementation.subroutines.finalize %}
    // - - - - - - - - - - - - - - - - - -FINISH SBRT CALL - - - - - - - - - - - - - - - - - - - - - - - - - -
    finish_{{actor_description.actor_name | lower }}_wrapper(&status_info);
    handle_status_info(status_info, "{{actor_description.actor_name}}");
    {% endif %}

    {% if code_description.implementation.code_parameters.parameters and code_description.implementation.code_parameters.schema %}
    // ------Deallocating code parameters ----------------------------
    free(xml_string);
    {% endif %}

    release_status_info(status_info);

{% if code_description.settings.mpi_compiler_cmd %}
    //----  MPI Finalization ----
    MPI_Finalized(&was_mpi_finalized);
    if (!was_mpi_finalized)
       MPI_Finalize();
{% endif %}
}
