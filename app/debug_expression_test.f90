program debug_expression_test
  use expression_analyzer
  use type_system
  implicit none
  
  type(type_info) :: result_type
  logical :: is_arith, is_comp, is_logical
  
  print *, 'Testing expression analyzer...'
  
  ! Test if "5" is detected as arithmetic
  is_arith = is_arithmetic_expression('5', result_type)
  print *, 'is_arithmetic_expression("5"): ', is_arith, ', base_type =', result_type%base_type
  
  ! Test if "5" is detected as comparison
  is_comp = is_comparison_expression('5', result_type)
  print *, 'is_comparison_expression("5"): ', is_comp, ', base_type =', result_type%base_type
  
  ! Test if "5" is detected as logical
  is_logical = is_logical_expression('5', result_type)
  print *, 'is_logical_expression("5"): ', is_logical, ', base_type =', result_type%base_type
  
  ! Test full analyze_expression
  call analyze_expression('5', result_type)
  print *, 'analyze_expression("5"): base_type =', result_type%base_type, ', kind =', result_type%kind
  
end program debug_expression_test