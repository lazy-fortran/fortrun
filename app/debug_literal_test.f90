program debug_literal_test
  use literal_analyzer
  use type_system
  implicit none
  
  type(type_info) :: result_type
  logical :: is_literal
  
  print *, 'Testing literal analyzer...'
  
  ! Test integer literal
  call analyze_literal('5', result_type, is_literal)
  print *, 'Literal "5": is_literal =', is_literal, ', base_type =', result_type%base_type, ', kind =', result_type%kind
  
  ! Test real literal
  call analyze_literal('5.0', result_type, is_literal)
  print *, 'Literal "5.0": is_literal =', is_literal, ', base_type =', result_type%base_type, ', kind =', result_type%kind
  
  print *, 'TYPE_INTEGER =', TYPE_INTEGER
  print *, 'TYPE_REAL =', TYPE_REAL
  
end program debug_literal_test