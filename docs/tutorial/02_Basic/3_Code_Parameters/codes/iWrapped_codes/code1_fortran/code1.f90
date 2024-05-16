module mod_code1


character(len=:), pointer :: greeting

contains


    !===================================================================
    !                            INIT
    !===================================================================
    subroutine code1_setup (code_parameters, status_code, status_message)
        use ids_schemas, only: ids_parameters_input
        use xml2eg_mdl,  only: xml2eg_parse_memory, xml2eg_get, type_xml2eg_document, xml2eg_free_doc, set_verbose
        implicit none

        type(ids_parameters_input), intent(in) :: code_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        ! This variable will contain the string from code parameters
        character(len=50) :: greeting_string

        ! This is a tree structure that is built from XML string
        ! This structure can be later on traversed with 'get' methods
        type(type_xml2eg_document) :: doc

        write(*,*)
        write(*,*)
        write(*,*) " --- Code Fortran: INIT ---"


        ! Parse the string with code parameters and save the result inside doc
        call xml2eg_parse_memory(code_parameters%parameters_value,doc)

        ! Get the calue of node 'greeting' and store it inside greeting_string
        ! variable; we can use it later on
        call xml2eg_get(doc,'greeting',greeting_string)

        ! Clean up after data are retrieved; This step can be done at the very
        ! end of your code as well.
        call xml2eg_free_doc(doc)

        ! Code specific initialization actions
        write(*,*) code_parameters%parameters_value
        ! SET GREETING TO value of <greeting> tag
        ! We will use this value later on inside code1_step routine
        allocate(character(60):: greeting)
        greeting = " Fortran: " // greeting_string

        ! Setting status to SUCCESS
        status_code = 0
        allocate(character(50):: status_message)
        status_message = 'OK'

    end subroutine code1_setup


    !===================================================================
    !                            MAIN
    !===================================================================
    subroutine code1_step(ids_in, ids_out, code_parameters, status_code, status_message)

        use ids_schemas, only: ids_core_profiles, ids_distribution_sources, ids_parameters_input
        use ids_routines

        implicit none

        type(ids_core_profiles):: ids_in
        type(ids_distribution_sources):: ids_out
        type(ids_parameters_input), intent(in) :: code_parameters
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        write(*,*)
        write(*,*) " Code1 Fortran: MAIN "

         ! Computations specific to the code
        ids_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS ! Mandatory field

        if (.not. associated(ids_out%ids_properties%comment)) allocate(ids_out%ids_properties%comment(1))
        ids_out%ids_properties%comment(1) = trim(ids_in%ids_properties%comment(1)) // trim(greeting) // " | "

        ! Setting status flag and message
        status_code = 0
        allocate(character(5):: status_message)
        status_message = 'OK'

    end subroutine code1_step


    !===================================================================
    !                            FINALIZE
    !===================================================================
    subroutine code1_cleanup(status_code, status_message)
        implicit none
        integer, intent(out) :: status_code
        character(len=:), pointer, intent(out) :: status_message

        write(*,*)
        write(*,*)
        write(*,*) " --- Code1 Fortran: FINALIZE ---"


        ! Code specific finalization actions
        deallocate(greeting)
        greeting => null()

        ! Setting status flag and messageE
        status_code = 0
        allocate(character(5):: status_message)
        status_message = 'OK'

    end subroutine code1_cleanup


end module mod_code1
