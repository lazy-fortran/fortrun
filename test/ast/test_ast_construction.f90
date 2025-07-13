program test_ast_construction
    use ast
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run tests
    if (.not. test_core_node_creation()) all_passed = .false.
    if (.not. test_sf_node_creation()) all_passed = .false.
    if (.not. test_ast_json_serialization()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All AST construction tests passed"
        stop 0
    else
        print '(a)', "Some AST construction tests failed"
        stop 1
    end if

contains

    logical function test_core_node_creation()
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        type(assignment_node) :: assign_node
        
        test_core_node_creation = .true.
        
        print '(a)', "Testing core AST node creation..."
        
        ! Create identifier node
        id_node = create_identifier("x", 1, 1)
        if (id_node%name /= "x") then
            print '(a)', "FAIL: Identifier name not set correctly"
            test_core_node_creation = .false.
        end if
        
        if (id_node%line /= 1 .or. id_node%column /= 1) then
            print '(a)', "FAIL: Identifier position not set correctly"
            test_core_node_creation = .false.
        end if
        
        ! Create literal node
        lit_node = create_literal("5", LITERAL_INTEGER, 1, 5)
        if (lit_node%value /= "5") then
            print '(a)', "FAIL: Literal value not set correctly"
            test_core_node_creation = .false.
        end if
        
        if (lit_node%literal_kind /= LITERAL_INTEGER) then
            print '(a)', "FAIL: Literal kind not set correctly"
            test_core_node_creation = .false.
        end if
        
        ! Create assignment node
        assign_node = create_assignment(id_node, lit_node, 1, 1)
        if (.not. allocated(assign_node%target)) then
            print '(a)', "FAIL: Assignment target not allocated"
            test_core_node_creation = .false.
        end if
        
        if (.not. allocated(assign_node%value)) then
            print '(a)', "FAIL: Assignment value not allocated"
            test_core_node_creation = .false.
        end if
        
        if (test_core_node_creation) then
            print '(a)', "PASS: Core AST node creation"
        end if
        
    end function test_core_node_creation

    logical function test_sf_node_creation()
        type(inferred_var_node) :: inferred_var
        type(sf_assignment_node) :: sf_assign
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        
        test_sf_node_creation = .true.
        
        print '(a)', "Testing Simple Fortran AST node creation..."
        
        ! Create basic nodes for composition
        id_node = create_identifier("y", 1, 1)
        lit_node = create_literal("3.14", LITERAL_REAL, 1, 5)
        
        ! Create Simple Fortran assignment
        sf_assign = create_sf_assignment(id_node, lit_node, inferred_type=.true., &
                                        inferred_type_name="real", line=1, column=1)
        
        if (.not. sf_assign%inferred_type) then
            print '(a)', "FAIL: SF assignment inferred_type flag not set"
            test_sf_node_creation = .false.
        end if
        
        if (sf_assign%inferred_type_name /= "real") then
            print '(a)', "FAIL: SF assignment inferred_type_name not set correctly"
            test_sf_node_creation = .false.
        end if
        
        ! Create inferred variable
        inferred_var = create_inferred_var("z", lit_node, 2, 1)
        if (inferred_var%name /= "z") then
            print '(a)', "FAIL: Inferred variable name not set correctly"
            test_sf_node_creation = .false.
        end if
        
        if (.not. allocated(inferred_var%initial_value)) then
            print '(a)', "FAIL: Inferred variable initial_value not allocated"
            test_sf_node_creation = .false.
        end if
        
        ! Note: Testing with arrays would require creating concrete arrays
        ! For now we test the simpler node creation
        
        if (test_sf_node_creation) then
            print '(a)', "PASS: Simple Fortran AST node creation"
        end if
        
    end function test_sf_node_creation

    logical function test_ast_json_serialization()
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        type(assignment_node) :: assign_node
        character(len=:), allocatable :: json_str
        character(len=*), parameter :: test_file = "test_ast.json"
        integer :: unit, iostat, file_size
        
        test_ast_json_serialization = .true.
        
        print '(a)', "Testing AST JSON serialization..."
        
        ! Create simple AST
        id_node = create_identifier("x", 1, 1)
        lit_node = create_literal("42", LITERAL_INTEGER, 1, 5)
        assign_node = create_assignment(id_node, lit_node, 1, 1)
        
        ! Test string serialization
        json_str = ast_to_json_string(assign_node)
        
        if (len(json_str) == 0) then
            print '(a)', "FAIL: JSON string is empty"
            test_ast_json_serialization = .false.
        end if
        
        if (index(json_str, '"type": "assignment"') == 0) then
            print '(a)', "FAIL: JSON missing assignment type"
            test_ast_json_serialization = .false.
        end if
        
        if (index(json_str, '"type": "identifier"') == 0) then
            print '(a)', "FAIL: JSON missing identifier type"
            test_ast_json_serialization = .false.
        end if
        
        if (index(json_str, '"type": "literal"') == 0) then
            print '(a)', "FAIL: JSON missing literal type"
            test_ast_json_serialization = .false.
        end if
        
        ! Test file serialization
        call ast_to_json_file(assign_node, test_file)
        
        inquire(file=test_file, size=file_size, iostat=iostat)
        if (iostat /= 0 .or. file_size <= 0) then
            print '(a)', "FAIL: JSON file not created or empty"
            test_ast_json_serialization = .false.
        else
            ! Clean up test file
            open(newunit=unit, file=test_file, status='old')
            close(unit, status='delete')
        end if
        
        if (test_ast_json_serialization) then
            print '(a)', "PASS: AST JSON serialization"
        end if
        
    end function test_ast_json_serialization

end program test_ast_construction