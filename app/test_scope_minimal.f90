program test_scope_minimal
    use scope_manager
    use type_system_hm
    implicit none

    type(scope_t) :: scope
    type(poly_type_t), allocatable :: result

    print *, "Creating scope..."
    scope = create_scope(SCOPE_GLOBAL, "test")
    print *, "Scope created"

    print *, "Testing direct scope lookup..."
    call scope%lookup("x", result)

    if (allocated(result)) then
        print *, "Found 'x' (unexpected)"
    else
        print *, "Did not find 'x' (expected)"
    end if

    print *, "Test completed successfully"

end program test_scope_minimal
