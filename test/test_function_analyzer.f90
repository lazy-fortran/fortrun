program test_function_analyzer
  use function_analyzer
  use type_system
  implicit none
  
  integer :: test_count, pass_count
  
  test_count = 0
  pass_count = 0
  
  print *, ' === Function Analyzer Tests ==='
  print *, ''
  
  call test_function_return_inference(test_count, pass_count)
  call test_intent_out_inference(test_count, pass_count)
  call test_subroutine_analysis(test_count, pass_count)
  call test_function_definition_parsing(test_count, pass_count)
  call test_call_site_analysis(test_count, pass_count)
  
  print *, ''
  if (pass_count == test_count) then
    print *, 'Function analyzer tests:', pass_count, '/', test_count, 'passed'
  else
    print *, 'Function analyzer tests: FAILED', pass_count, '/', test_count
    stop 1
  end if
  
contains

  subroutine test_function_return_inference(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    
    type(type_info) :: result_type
    character(len=*), parameter :: func_def = &
      'integer function calculate_sum(a, b)' // new_line('A') // &
      '  integer, intent(in) :: a, b' // new_line('A') // &
      '  calculate_sum = a + b' // new_line('A') // &
      'end function calculate_sum'
    
    print *, ' Testing function return type inference...'
    
    ! Test 1: Integer function return
    test_count = test_count + 1
    call analyze_function_return('result = calculate_sum(x, y)', func_def, result_type)
    if (result_type%base_type == TYPE_INTEGER .and. result_type%kind == 4) then
      print *, '  ✓ PASS: Integer function return'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Integer function return'
    end if
    
    ! Test 2: Real function return
    test_count = test_count + 1
    call analyze_function_return('value = sqrt(x)', '', result_type)
    if (result_type%base_type == TYPE_REAL .and. result_type%kind == 8) then
      print *, '  ✓ PASS: Real function return (intrinsic)'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Real function return (intrinsic)'
    end if
    
    ! Test 3: Logical function return
    test_count = test_count + 1
    call analyze_function_return('flag = is_valid(input)', &
      'logical function is_valid(x)' // new_line('A') // 'end function is_valid', result_type)
    if (result_type%base_type == TYPE_LOGICAL .and. result_type%kind == 4) then
      print *, '  ✓ PASS: Logical function return'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Logical function return'
    end if
    
  end subroutine test_function_return_inference

  subroutine test_intent_out_inference(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    
    type(type_info) :: result_type
    character(len=*), parameter :: sub_def = &
      'subroutine get_values(input, output1, output2)' // new_line('A') // &
      '  integer, intent(in) :: input' // new_line('A') // &
      '  real(8), intent(out) :: output1' // new_line('A') // &
      '  logical, intent(out) :: output2' // new_line('A') // &
      'end subroutine get_values'
    
    print *, ' Testing intent(out) argument inference...'
    
    ! Test 1: Real intent(out) argument
    test_count = test_count + 1
    call analyze_intent_out_argument('call get_values(x, result1, result2)', 'result1', sub_def, result_type)
    if (result_type%base_type == TYPE_REAL .and. result_type%kind == 8) then
      print *, '  ✓ PASS: Real intent(out) argument'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Real intent(out) argument'
    end if
    
    ! Test 2: Logical intent(out) argument
    test_count = test_count + 1
    call analyze_intent_out_argument('call get_values(x, result1, result2)', 'result2', sub_def, result_type)
    if (result_type%base_type == TYPE_LOGICAL .and. result_type%kind == 4) then
      print *, '  ✓ PASS: Logical intent(out) argument'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Logical intent(out) argument'
    end if
    
    ! Test 3: Intent(in) argument (should return unknown)
    test_count = test_count + 1
    call analyze_intent_out_argument('call get_values(x, result1, result2)', 'x', sub_def, result_type)
    if (result_type%base_type == TYPE_UNKNOWN) then
      print *, '  ✓ PASS: Intent(in) argument returns unknown'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Intent(in) argument should return unknown'
    end if
    
  end subroutine test_intent_out_inference

  subroutine test_subroutine_analysis(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    
    logical :: is_sub
    
    print *, ' Testing subroutine vs function detection...'
    
    ! Test 1: Detect subroutine call
    test_count = test_count + 1
    call is_subroutine_call('call my_subroutine(a, b)', is_sub)
    if (is_sub) then
      print *, '  ✓ PASS: Subroutine call detection'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Subroutine call detection'
    end if
    
    ! Test 2: Detect function call (assignment)
    test_count = test_count + 1
    call is_subroutine_call('result = my_function(a, b)', is_sub)
    if (.not. is_sub) then
      print *, '  ✓ PASS: Function call detection'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Function call detection'
    end if
    
  end subroutine test_subroutine_analysis

  subroutine test_function_definition_parsing(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    
    character(len=64) :: func_name
    type(type_info) :: return_type
    character(len=*), parameter :: func_def = 'real(8) function compute_area(radius)'
    
    print *, ' Testing function definition parsing...'
    
    ! Test 1: Parse function name and return type
    test_count = test_count + 1
    call parse_function_definition(func_def, func_name, return_type)
    if (trim(func_name) == 'compute_area' .and. &
        return_type%base_type == TYPE_REAL .and. return_type%kind == 8) then
      print *, '  ✓ PASS: Function definition parsing'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Function definition parsing'
      print *, '    Expected: compute_area, TYPE_REAL(8)'
      print *, '    Got: "' // trim(func_name) // '", TYPE=', return_type%base_type, ', KIND=', return_type%kind
    end if
    
  end subroutine test_function_definition_parsing

  subroutine test_call_site_analysis(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    
    character(len=64) :: func_name, var_name
    
    print *, ' Testing call site analysis...'
    
    ! Test 1: Extract function name from call
    test_count = test_count + 1
    call extract_function_call('result = calculate_something(x, y, z)', func_name, var_name)
    if (trim(func_name) == 'calculate_something' .and. trim(var_name) == 'result') then
      print *, '  ✓ PASS: Function call extraction'
      pass_count = pass_count + 1
    else
      print *, '  ✗ FAIL: Function call extraction'
    end if
    
  end subroutine test_call_site_analysis

end program test_function_analyzer