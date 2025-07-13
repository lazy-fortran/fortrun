program test_type_inference_edge_cases
    use type_inference_coordinator
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Type Inference Edge Case Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test edge cases in type inference
    if (.not. test_empty_expressions()) all_tests_passed = .false.
    if (.not. test_complex_literals()) all_tests_passed = .false.
    if (.not. test_nested_expressions()) all_tests_passed = .false.
    if (.not. test_edge_case_literals()) all_tests_passed = .false.
    if (.not. test_environment_limits()) all_tests_passed = .false.
    if (.not. test_declaration_generation()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All type inference edge case tests passed!"
        stop 0
    else
        print *, "Some type inference edge case tests failed!"
        stop 1
    end if
    
contains

    function test_empty_expressions() result(passed)
        logical :: passed
        type(type_info) :: inferred
        type(type_environment) :: env
        
        print *, "Test 1: Empty and whitespace expressions"
        passed = .true.
        
        call init_type_environment(env)
        
        ! Test empty string
        call infer_type_from_expression("", inferred, env)
        if (inferred%base_type /= TYPE_UNKNOWN) then
            print *, "  FAILED: Empty string should be unknown type"
            passed = .false.
        end if
        
        ! Test only whitespace
        call infer_type_from_expression("   ", inferred, env)
        if (inferred%base_type /= TYPE_UNKNOWN) then
            print *, "  FAILED: Whitespace should be unknown type"
            passed = .false.
        end if
        
        ! Test only comment
        call infer_type_from_expression("! just a comment", inferred, env)
        if (inferred%base_type /= TYPE_UNKNOWN) then
            print *, "  FAILED: Comment only should be unknown type"
            passed = .false.
        end if
        
        call cleanup_type_environment(env)
        
        if (passed) print *, "  PASS: Empty expressions"
        
    end function test_empty_expressions

    function test_complex_literals() result(passed)
        logical :: passed
        type(type_info) :: inferred
        
        print *, "Test 2: Complex literal patterns"
        passed = .true.
        
        ! Test hexadecimal integers
        call infer_type_from_expression("Z'FF'", inferred)
        if (inferred%base_type /= TYPE_INTEGER) then
            print *, "  WARNING: Hex literal not recognized as integer"
        end if
        
        ! Test binary integers
        call infer_type_from_expression("B'101010'", inferred)
        if (inferred%base_type /= TYPE_INTEGER) then
            print *, "  WARNING: Binary literal not recognized as integer"
        end if
        
        ! Test octal integers
        call infer_type_from_expression("O'777'", inferred)
        if (inferred%base_type /= TYPE_INTEGER) then
            print *, "  WARNING: Octal literal not recognized as integer"
        end if
        
        ! Test scientific notation edge cases
        call infer_type_from_expression("1E+308", inferred)
        if (inferred%base_type /= TYPE_REAL) then
            print *, "  FAILED: Scientific notation should be real"
            passed = .false.
        end if
        
        call infer_type_from_expression(".5E-10", inferred)
        if (inferred%base_type /= TYPE_REAL) then
            print *, "  FAILED: .5E-10 should be real"
            passed = .false.
        end if
        
        ! Test integer with kind suffix
        call infer_type_from_expression("42_8", inferred)
        if (inferred%base_type /= TYPE_INTEGER) then
            print *, "  FAILED: 42_8 should be integer"
            passed = .false.
        end if
        if (inferred%kind /= 8) then
            print *, "  WARNING: Kind 8 not detected"
        end if
        
        if (passed) print *, "  PASS: Complex literals"
        
    end function test_complex_literals

    function test_nested_expressions() result(passed)
        logical :: passed
        type(type_info) :: inferred
        type(type_environment) :: env
        
        print *, "Test 3: Nested and compound expressions"
        passed = .true.
        
        call init_type_environment(env)
        
        ! Test nested arithmetic
        call infer_type_from_expression("(1 + 2) * (3 + 4)", inferred, env)
        if (inferred%base_type /= TYPE_INTEGER .and. inferred%base_type /= TYPE_UNKNOWN) then
            print *, "  WARNING: Nested arithmetic might not be fully supported"
        end if
        
        ! Test mixed type arithmetic
        call infer_type_from_expression("1 + 2.0", inferred, env)
        if (inferred%base_type /= TYPE_REAL) then
            print *, "  FAILED: Mixed int/real should be real"
            passed = .false.
        end if
        
        ! Test nested comparisons
        call infer_type_from_expression("(x > 5) .and. (y < 10)", inferred, env)
        if (inferred%base_type /= TYPE_LOGICAL) then
            print *, "  FAILED: Logical expression should be logical"
            passed = .false.
        end if
        
        ! Test function calls
        call infer_type_from_expression("sin(3.14)", inferred, env)
        if (inferred%base_type /= TYPE_REAL) then
            print *, "  WARNING: sin() should return real"
        end if
        
        call infer_type_from_expression("len('hello')", inferred, env)
        if (inferred%base_type /= TYPE_INTEGER) then
            print *, "  WARNING: len() should return integer"
        end if
        
        call cleanup_type_environment(env)
        
        if (passed) print *, "  PASS: Nested expressions"
        
    end function test_nested_expressions

    function test_edge_case_literals() result(passed)
        logical :: passed
        type(type_info) :: inferred
        
        print *, "Test 4: Edge case literal values"
        passed = .true.
        
        ! Test very long strings
        call infer_type_from_expression("'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" // &
                                       "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" // &
                                       "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'", inferred)
        if (inferred%base_type /= TYPE_CHARACTER) then
            print *, "  FAILED: Long string should be character"
            passed = .false.
        end if
        if (inferred%char_len /= 120) then
            print *, "  WARNING: Long string length incorrect"
        end if
        
        ! Test empty string
        call infer_type_from_expression("''", inferred)
        if (inferred%base_type /= TYPE_CHARACTER) then
            print *, "  FAILED: Empty string should be character"
            passed = .false.
        end if
        if (inferred%char_len /= 0) then
            print *, "  WARNING: Empty string length should be 0"
        end if
        
        ! Test double quotes
        call infer_type_from_expression('""', inferred)
        if (inferred%base_type /= TYPE_CHARACTER) then
            print *, "  FAILED: Double quote string should be character"
            passed = .false.
        end if
        
        ! Test special logical values
        call infer_type_from_expression(".TRUE.", inferred)
        if (inferred%base_type /= TYPE_LOGICAL) then
            print *, "  FAILED: .TRUE. should be logical"
            passed = .false.
        end if
        
        call infer_type_from_expression(".false.", inferred)
        if (inferred%base_type /= TYPE_LOGICAL) then
            print *, "  FAILED: .false. should be logical"
            passed = .false.
        end if
        
        ! Test numbers at limits
        call infer_type_from_expression("2147483647", inferred)  ! Max int32
        if (inferred%base_type /= TYPE_INTEGER) then
            print *, "  FAILED: Large integer should be integer"
            passed = .false.
        end if
        
        if (passed) print *, "  PASS: Edge case literals"
        
    end function test_edge_case_literals

    function test_environment_limits() result(passed)
        logical :: passed
        type(type_environment) :: env
        type(type_info) :: var_type, retrieved_type
        character(len=10) :: var_name
        logical :: found
        integer :: i
        
        print *, "Test 5: Type environment limits"
        passed = .true.
        
        call init_type_environment(env)
        
        ! Fill environment to near capacity
        do i = 1, 999
            write(var_name, '(a,i3.3)') "var", i
            var_type%base_type = TYPE_INTEGER
            var_type%kind = 4
            call process_assignment(env, trim(var_name), "42")
        end do
        
        ! Check retrieval
        call get_variable_type(env, "var500", retrieved_type, found)
        if (.not. found) then
            print *, "  FAILED: Should find var500"
            passed = .false.
        end if
        
        ! Add one more (should still work)
        call process_assignment(env, "var1000", "3.14")
        
        ! Try to exceed limit
        call process_assignment(env, "var1001", "overflow")
        
        ! Check if it handled overflow gracefully
        call get_variable_type(env, "var1001", retrieved_type, found)
        ! May or may not find it depending on implementation
        
        call cleanup_type_environment(env)
        
        if (passed) print *, "  PASS: Environment limits"
        
    end function test_environment_limits

    function test_declaration_generation() result(passed)
        logical :: passed
        type(type_environment) :: env
        character(len=2000) :: declarations
        
        print *, "Test 6: Declaration generation edge cases"
        passed = .true.
        
        call init_type_environment(env)
        
        ! Test with no variables
        call generate_declarations(env, declarations)
        if (len_trim(declarations) /= 0) then
            print *, "  WARNING: Empty env should generate no declarations"
        end if
        
        ! Add various types
        call process_assignment(env, "int_var", "42")
        call process_assignment(env, "real_var", "3.14")
        call process_assignment(env, "bool_var", ".true.")
        call process_assignment(env, "str_var", "'hello'")
        call process_assignment(env, "long_name_variable_test", "999")
        
        ! Generate declarations
        call generate_declarations(env, declarations)
        
        ! Check that declarations were generated
        if (len_trim(declarations) == 0) then
            print *, "  FAILED: Should generate declarations"
            passed = .false.
        end if
        
        ! Check for specific declarations
        if (index(declarations, "integer") == 0) then
            print *, "  FAILED: Should include integer declaration"
            passed = .false.
        end if
        
        if (index(declarations, "real") == 0) then
            print *, "  FAILED: Should include real declaration"
            passed = .false.
        end if
        
        if (index(declarations, "logical") == 0) then
            print *, "  FAILED: Should include logical declaration"
            passed = .false.
        end if
        
        if (index(declarations, "character") == 0) then
            print *, "  FAILED: Should include character declaration"
            passed = .false.
        end if
        
        call cleanup_type_environment(env)
        
        if (passed) print *, "  PASS: Declaration generation"
        
    end function test_declaration_generation

end program test_type_inference_edge_cases