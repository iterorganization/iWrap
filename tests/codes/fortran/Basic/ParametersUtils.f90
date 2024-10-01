module mod_parameters_utils

implicit none
    contains
    !
    !    HELPER SUBROUTINES
    !
    subroutine extract_xml_values(parameters_string, ntimes, multiplication_factor)
        use xml2eg_mdl, only: xml2eg_parse_memory, xml2eg_get, type_xml2eg_document, xml2eg_free_doc, set_verbose
        implicit none

        character(len=:), allocatable, intent(IN) :: parameters_string
        integer, intent(INOUT) :: ntimes
        real, intent(INOUT) :: multiplication_factor
        type(type_xml2eg_document) :: doc

        character(len=132), pointer :: codeparam_string(:)
        allocate(codeparam_string(1))
        codeparam_string(1) = parameters_string

        call xml2eg_parse_memory(codeparam_string,doc)
        call xml2eg_get(doc,'ntimes',ntimes)
        call xml2eg_get(doc,'multiplication_factor',multiplication_factor)
        call xml2eg_free_doc(doc)

    end subroutine extract_xml_values

    subroutine extract_json_values(parameters_string, ntimes, multiplication_factor)
        use json_module
        use json_value_module
        implicit none

        character(len=:), allocatable, intent(IN) :: parameters_string
        integer, intent(INOUT) :: ntimes
        real, intent(INOUT) :: multiplication_factor
        type(json_file) :: json

        call json%deserialize(parameters_string)

        call json%get('parameters.ntimes', ntimes)
        call json%get('parameters.multiplication_factor', multiplication_factor)

    end subroutine extract_json_values
    ! -----------------------------------------------------------------------------------------------------------------------------

    subroutine extract_namelist_values(parameters_string, ntimes, multiplication_factor)
        implicit none

        character(len=:), allocatable, intent(IN) :: parameters_string
        integer, intent(INOUT) :: ntimes
        real, intent(INOUT) :: multiplication_factor
        namelist /parameters/ ntimes, multiplication_factor

        read(parameters_string, nml=parameters)



    end subroutine extract_namelist_values
! -----------------------------------------------------------------------------------------------------------------------------

    subroutine extract_values(parameters_string, ntimes, multiplication_factor)
        implicit none

        character(len=:), allocatable, intent(IN) :: parameters_string
        integer, intent(out) :: ntimes
        real, intent(out) :: multiplication_factor

        if (.not. allocated(parameters_string)) then
            return
        end if

        if (parameters_string(1:1) == '<') then
            !XML
            print *, 'PARSING XML'
            call extract_xml_values(parameters_string, ntimes, multiplication_factor)
        else if (parameters_string(1:1) == '{') then
            !JSON
            print *, 'PARSING JSON'
            call extract_json_values(parameters_string, ntimes, multiplication_factor)
        else if (parameters_string(1:1) == '&') then
            !NAMELIST
            print *, 'PARSING NAMELIST'
            call extract_namelist_values(parameters_string, ntimes, multiplication_factor)
        else
            print *, 'Could not recognise parameters format'
        endif

    end subroutine extract_values

end module mod_parameters_utils