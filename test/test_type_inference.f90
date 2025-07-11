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
  call test_multiple_assignments(test_count, pass_count)
  call test_declaration_generation(test_count, pass_count)
  
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

end program test_type_inference