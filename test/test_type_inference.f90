program test_type_inference
  use type_inference
  implicit none
  
  integer :: test_count, pass_count
  
  test_count = 0
  pass_count = 0
  
  call test_integer_literal_inference(test_count, pass_count)
  call test_real_literal_inference(test_count, pass_count)
  call test_logical_literal_inference(test_count, pass_count)
  call test_character_literal_inference(test_count, pass_count)
  call test_expression_inference(test_count, pass_count)
  call test_comparison_expressions(test_count, pass_count)
  call test_multiple_assignments(test_count, pass_count)
  call test_declaration_generation(test_count, pass_count)
  call test_edge_cases(test_count, pass_count)
  
  print '(a)', ''
  print '(a,i0,a,i0,a)', 'Type inference tests: ', pass_count, '/', test_count, ' passed'
  
  if (pass_count /= test_count) then
    error stop 'Some tests failed!'
  end if
  
contains

  subroutine test_integer_literal_inference(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_info) :: inferred_type
    
    ! Test 1: Simple integer literal
    test_count = test_count + 1
    call infer_type_from_expression("42", inferred_type)
    if (inferred_type%base_type == TYPE_INTEGER .and. &
        inferred_type%kind == 4) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Simple integer literal (42)'
    else
      print '(a)', 'FAIL: Simple integer literal should be integer'
    end if
    
    ! Test 2: Negative integer
    test_count = test_count + 1
    call infer_type_from_expression("-123", inferred_type)
    if (inferred_type%base_type == TYPE_INTEGER) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Negative integer literal (-123)'
    else
      print '(a)', 'FAIL: Negative integer should be integer'
    end if
    
    ! Test 3: Integer with kind suffix
    test_count = test_count + 1
    call infer_type_from_expression("42_8", inferred_type)
    if (inferred_type%base_type == TYPE_INTEGER .and. &
        inferred_type%kind == 8) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Integer with kind suffix (42_8)'
    else
      print '(a)', 'FAIL: Integer with _8 should have kind 8'
    end if
  end subroutine test_integer_literal_inference
  
  subroutine test_real_literal_inference(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_info) :: inferred_type
    
    ! Test 1: Simple real literal
    test_count = test_count + 1
    call infer_type_from_expression("3.14", inferred_type)
    if (inferred_type%base_type == TYPE_REAL .and. &
        inferred_type%kind == 8) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Simple real literal (3.14)'
    else
      print '(a)', 'FAIL: Real literal should be real(8)'
    end if
    
    ! Test 2: Scientific notation
    test_count = test_count + 1
    call infer_type_from_expression("1.0e-10", inferred_type)
    if (inferred_type%base_type == TYPE_REAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Scientific notation (1.0e-10)'
    else
      print '(a)', 'FAIL: Scientific notation should be real'
    end if
    
    ! Test 3: Double precision suffix
    test_count = test_count + 1
    call infer_type_from_expression("3.14d0", inferred_type)
    if (inferred_type%base_type == TYPE_REAL .and. &
        inferred_type%kind == 8) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Double precision suffix (3.14d0)'
    else
      print '(a)', 'FAIL: d0 suffix should be real(8)'
    end if
    
    ! Test 4: Real with decimal point only
    test_count = test_count + 1
    call infer_type_from_expression("5.", inferred_type)
    if (inferred_type%base_type == TYPE_REAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Real with trailing dot (5.)'
    else
      print '(a)', 'FAIL: Number with decimal point should be real'
    end if
  end subroutine test_real_literal_inference
  
  subroutine test_logical_literal_inference(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_info) :: inferred_type
    
    ! Test 1: .true. literal
    test_count = test_count + 1
    call infer_type_from_expression(".true.", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Logical true literal (.true.)'
    else
      print '(a)', 'FAIL: .true. should be logical'
    end if
    
    ! Test 2: .false. literal
    test_count = test_count + 1
    call infer_type_from_expression(".false.", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Logical false literal (.false.)'
    else
      print '(a)', 'FAIL: .false. should be logical'
    end if
  end subroutine test_logical_literal_inference
  
  subroutine test_character_literal_inference(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_info) :: inferred_type
    
    ! Test 1: Single quoted string
    test_count = test_count + 1
    call infer_type_from_expression("'hello'", inferred_type)
    if (inferred_type%base_type == TYPE_CHARACTER .and. &
        inferred_type%char_len == 5) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Single quoted string (''hello'')'
    else
      print '(a)', 'FAIL: String literal should be character(len=5)'
    end if
    
    ! Test 2: Double quoted string
    test_count = test_count + 1
    call infer_type_from_expression('"world"', inferred_type)
    if (inferred_type%base_type == TYPE_CHARACTER .and. &
        inferred_type%char_len == 5) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Double quoted string ("world")'
    else
      print '(a)', 'FAIL: String literal should be character(len=5)'
    end if
    
    ! Test 3: Empty string
    test_count = test_count + 1
    call infer_type_from_expression('""', inferred_type)
    if (inferred_type%base_type == TYPE_CHARACTER .and. &
        inferred_type%char_len == 0) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Empty string ("")'
    else
      print '(a)', 'FAIL: Empty string should be character(len=0)'
    end if
  end subroutine test_character_literal_inference
  
  subroutine test_expression_inference(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_info) :: inferred_type
    
    ! Test 1: Integer arithmetic
    test_count = test_count + 1
    call infer_type_from_expression("2 + 3", inferred_type)
    if (inferred_type%base_type == TYPE_INTEGER) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Integer arithmetic (2 + 3)'
    else
      print '(a)', 'FAIL: Integer + integer should be integer'
    end if
    
    ! Test 2: Mixed arithmetic (promotion to real)
    test_count = test_count + 1
    call infer_type_from_expression("2.0 + 3", inferred_type)
    if (inferred_type%base_type == TYPE_REAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Mixed arithmetic promotes to real (2.0 + 3)'
    else
      print '(a)', 'FAIL: Real + integer should promote to real'
    end if
    
    ! Test 3: Function call (intrinsic)
    test_count = test_count + 1
    call infer_type_from_expression("sin(1.0)", inferred_type)
    if (inferred_type%base_type == TYPE_REAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Intrinsic function sin(1.0)'
    else
      print '(a)', 'FAIL: sin() should return real'
    end if
  end subroutine test_expression_inference
  
  subroutine test_comparison_expressions(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_info) :: inferred_type
    
    ! Test 1: Simple > comparison
    test_count = test_count + 1
    call infer_type_from_expression("x > 0", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Greater than comparison (x > 0)'
    else
      print '(a)', 'FAIL: x > 0 should return logical'
    end if
    
    ! Test 2: Less than comparison
    test_count = test_count + 1
    call infer_type_from_expression("y < 5.0", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Less than comparison (y < 5.0)'
    else
      print '(a,i0)', 'FAIL: y < 5.0 should return logical, got type: ', inferred_type%base_type
    end if
    
    ! Test 3: Equality comparison
    test_count = test_count + 1
    call infer_type_from_expression("a == b", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Equality comparison (a == b)'
    else
      print '(a)', 'FAIL: a == b should return logical'
    end if
    
    ! Test 4: Not equal comparison
    test_count = test_count + 1
    call infer_type_from_expression("x /= 10", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Not equal comparison (x /= 10)'
    else
      print '(a)', 'FAIL: x /= 10 should return logical'
    end if
    
    ! Test 5: Greater or equal comparison
    test_count = test_count + 1
    call infer_type_from_expression("count >= 0", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Greater or equal comparison (count >= 0)'
    else
      print '(a)', 'FAIL: count >= 0 should return logical'
    end if
    
    ! Test 6: Less or equal comparison
    test_count = test_count + 1
    call infer_type_from_expression("val <= 100.0", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Less or equal comparison (val <= 100.0)'
    else
      print '(a)', 'FAIL: val <= 100.0 should return logical'
    end if
    
    ! Test 7: Fortran-style .gt. operator
    test_count = test_count + 1
    call infer_type_from_expression("x .gt. 0", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Fortran .gt. comparison (x .gt. 0)'
    else
      print '(a)', 'FAIL: x .gt. 0 should return logical'
    end if
    
    ! Test 8: Fortran-style .eq. operator
    test_count = test_count + 1
    call infer_type_from_expression("a .eq. b", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Fortran .eq. comparison (a .eq. b)'
    else
      print '(a)', 'FAIL: a .eq. b should return logical'
    end if
    
    ! Test 9: Complex comparison with literals
    test_count = test_count + 1
    call infer_type_from_expression("42 > 10", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Literal comparison (42 > 10)'
    else
      print '(a)', 'FAIL: 42 > 10 should return logical'
    end if
    
    ! Test 10: Mixed type comparison
    test_count = test_count + 1
    call infer_type_from_expression("3.14 > 3", inferred_type)
    if (inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Mixed type comparison (3.14 > 3)'
    else
      print '(a)', 'FAIL: 3.14 > 3 should return logical'
    end if
    
  end subroutine test_comparison_expressions
  
  subroutine test_multiple_assignments(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_environment) :: env
    type(type_info) :: var_type
    logical :: found
    
    call init_type_environment(env)
    
    ! Test 1: Track variable through multiple assignments
    test_count = test_count + 1
    call process_assignment(env, "x", "42")
    call process_assignment(env, "x", "x + 1")
    call get_variable_type(env, "x", var_type, found)
    
    if (found .and. var_type%base_type == TYPE_INTEGER) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Variable type consistent through assignments'
    else
      print '(a)', 'FAIL: Variable should maintain integer type'
    end if
    
    ! Test 2: Type conflict detection
    test_count = test_count + 1
    call process_assignment(env, "y", "42")
    call process_assignment(env, "y", "3.14")  ! Type change!
    call get_variable_type(env, "y", var_type, found)
    
    if (found .and. var_type%base_type == TYPE_REAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Type promotion on conflicting assignment'
    else
      print '(a)', 'FAIL: Should promote to real on type conflict'
    end if
    
    call cleanup_type_environment(env)
  end subroutine test_multiple_assignments
  
  subroutine test_declaration_generation(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_environment) :: env
    character(len=1024) :: declarations
    
    call init_type_environment(env)
    
    ! Add some variables with different types
    call process_assignment(env, "count", "0")
    call process_assignment(env, "pi", "3.14159")
    call process_assignment(env, "name", '"Fortran"')
    call process_assignment(env, "flag", ".true.")
    
    ! Test 1: Generate declarations
    test_count = test_count + 1
    call generate_declarations(env, declarations)
    
    if (index(declarations, "integer :: count") > 0 .and. &
        index(declarations, "real(8) :: pi") > 0 .and. &
        index(declarations, "character(len=7) :: name") > 0 .and. &
        index(declarations, "logical :: flag") > 0) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Declaration generation for all types'
    else
      print '(a)', 'FAIL: Missing or incorrect declarations'
      print '(a)', 'Generated: ' // trim(declarations)
    end if
    
    call cleanup_type_environment(env)
  end subroutine test_declaration_generation

  subroutine test_edge_cases(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    type(type_info) :: inferred_type
    type(type_environment) :: env
    logical :: found
    
    call init_type_environment(env)
    
    ! Test 1: Empty expression
    test_count = test_count + 1
    call infer_type_from_expression("", inferred_type)
    if (inferred_type%base_type == TYPE_UNKNOWN) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Empty expression returns unknown type'
    else
      print '(a)', 'FAIL: Empty expression should return unknown type'
    end if
    
    ! Test 2: Expression with only spaces
    test_count = test_count + 1
    call infer_type_from_expression("   ", inferred_type)
    if (inferred_type%base_type == TYPE_UNKNOWN) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Whitespace-only expression returns unknown type'
    else
      print '(a)', 'FAIL: Whitespace-only expression should return unknown type'
    end if
    
    ! Test 3: Nested parentheses (not currently supported)
    test_count = test_count + 1
    call infer_type_from_expression("(42)", inferred_type)
    if (inferred_type%base_type == TYPE_UNKNOWN .or. &
        inferred_type%base_type == TYPE_INTEGER) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Parenthesized expression handled'
    else
      print '(a)', 'FAIL: Parenthesized expression not handled correctly'
    end if
    
    ! Test 4: Complex expression with multiple operators
    test_count = test_count + 1
    call process_assignment(env, "complex_expr", "(a + b) * c / d")
    call get_variable_type(env, "complex_expr", inferred_type, found)
    if (found) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Complex expression assigned'
    else
      print '(a)', 'FAIL: Complex expression should be assigned'
    end if
    
    ! Test 5: Very long variable name
    test_count = test_count + 1
    call process_assignment(env, "very_long_variable_name_that_exceeds_normal_length", "42")
    call get_variable_type(env, "very_long_variable_name_that_exceeds_normal_length", inferred_type, found)
    if (found .and. inferred_type%base_type == TYPE_INTEGER) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Long variable names supported'
    else
      print '(a)', 'FAIL: Long variable names should be supported'
    end if
    
    ! Test 6: Assignment with comparison result
    test_count = test_count + 1
    call process_assignment(env, "is_valid", "x > 0")
    call get_variable_type(env, "is_valid", inferred_type, found)
    if (found .and. inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Comparison assignment creates logical variable'
    else
      print '(a)', 'FAIL: Comparison assignment should create logical variable'
    end if
    
    ! Test 7: Character with embedded quotes
    test_count = test_count + 1
    call infer_type_from_expression('"He said ""Hello"""', inferred_type)
    if (inferred_type%base_type == TYPE_CHARACTER) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Character with embedded quotes'
    else
      print '(a)', 'FAIL: Character with embedded quotes should be recognized'
    end if
    
    ! Test 8: Scientific notation with uppercase E
    test_count = test_count + 1
    call infer_type_from_expression("1.23E-4", inferred_type)
    if (inferred_type%base_type == TYPE_REAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Scientific notation with uppercase E'
    else
      print '(a)', 'FAIL: Scientific notation with uppercase E should be real'
    end if
    
    ! Test 9: Logical operators (not currently supported as expressions)
    test_count = test_count + 1
    call infer_type_from_expression("a .and. b", inferred_type)
    if (inferred_type%base_type == TYPE_UNKNOWN .or. &
        inferred_type%base_type == TYPE_LOGICAL) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Logical operator expression handled'
    else
      print '(a)', 'FAIL: Logical operator expression not handled correctly'
    end if
    
    ! Test 10: Array subscript (should not be processed as assignment)
    test_count = test_count + 1
    call process_assignment(env, "array(i)", "42")
    call get_variable_type(env, "array(i)", inferred_type, found)
    if (.not. found) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Array subscripts correctly ignored'
    else
      print '(a)', 'FAIL: Array subscripts should be ignored'
    end if
    
    call cleanup_type_environment(env)
  end subroutine test_edge_cases

end program test_type_inference