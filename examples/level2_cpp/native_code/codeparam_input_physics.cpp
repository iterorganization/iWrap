#include "codeparam_input_physics.h"

void assign_codeparam(char* codeparam_string, codeparam_physics_data_t &codeparam_data)
{
    codeparam_data.ntimes = 2;
    codeparam_data.multiplication_factor = 1.5;
/*
    use xml2eg_mdl, only: xml2eg_parse_memory, xml2eg_get, type_xml2eg_document, xml2eg_free_doc, set_verbose
    
    ! Input/Output
    character(len=132), pointer :: codeparam_string(:)
    type(type_codeparam_physics_data), intent(out) :: codeparam_physics

    ! Internal
    type(type_xml2eg_document) :: doc

    ! Parse the "codeparam_string". This means that the data is put into a document "doc"
    call xml2eg_parse_memory(codeparam_string,doc)
    call set_verbose(.TRUE.) ! Only needed if you want to see what's going on in the parsing
    
    call xml2eg_get(doc,'ntimes',codeparam_physics%ntimes)
    call xml2eg_get(doc,'multiplication_factor',codeparam_physics%multiplication_factor)

    ! Make sure to clean up after you!!
    ! When calling "xml2eg_parse_memory" memory was allocated in the "doc" object.
    ! This memory is freed by "xml2eg_free_doc(doc)"
    call xml2eg_free_doc(doc)
*/
}
