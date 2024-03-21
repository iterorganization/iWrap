program code0

    use ids_schemas, only: ids_distribution_sources
    use ids_routines

    implicit none

    type(ids_distribution_sources):: ids_out

    CALL getIDS(ids_out)

    write(*,*) "Message: ", ids_out%ids_properties%comment(1)


contains
    !===================================================================
    !                            MAIN
    !===================================================================
    function greet()
      character(:), allocatable :: greet

      allocate(character(5) :: greet)
      greet = 'Hello from Fortan IDS!'

    end function greet
  
    subroutine getIDS(ids_out)

        use ids_schemas, only: ids_distribution_sources
        use ids_routines

        implicit none

        type(ids_distribution_sources):: ids_out
        character(:), allocatable :: greet_msg

        greet_msg = greet()

        ids_out%ids_properties%homogeneous_time = IDS_TIME_MODE_HETEROGENEOUS ! Mandatory field

        if (.not. associated(ids_out%ids_properties%comment)) allocate(ids_out%ids_properties%comment(1))   
        ids_out%ids_properties%comment(1) = trim(greet_msg)

    end subroutine getIDS

end program code0
