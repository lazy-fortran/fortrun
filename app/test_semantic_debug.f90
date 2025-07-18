program test_semantic_debug
    use semantic_analyzer
    use type_system_hm
    use scope_manager
    implicit none

    type(semantic_context_t) :: ctx
    type(scope_stack_t) :: stack
    type(poly_type_t), allocatable :: result

    print *, "Creating semantic context..."
    ctx = create_semantic_context()
    print *, "Context created successfully"

    print *, "Testing scope lookup..."
    call ctx%scopes%lookup("x", result)

    if (allocated(result)) then
        print *, "Found 'x' (unexpected)"
    else
        print *, "Did not find 'x' (expected)"
    end if

    print *, "Test completed successfully"

end program test_semantic_debug
