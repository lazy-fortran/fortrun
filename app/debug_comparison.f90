program debug_comparison
  use type_inference
  implicit none
  
  type(type_info) :: result
  character(len=256) :: expr
  integer :: pos
  
  ! Test the exact failing case
  expr = "y < 5.0"
  print *, 'Testing:', trim(expr)
  pos = index(expr, '<')
  print *, '  Position of <:', pos
  call infer_type_from_expression(expr, result)
  print *, '  Type:', result%base_type, ' (expected 3 for logical)'
  print *
  
  ! Test working case
  expr = "x > 0"
  print *, 'Testing:', trim(expr)
  pos = index(expr, '>')
  print *, '  Position of >:', pos
  call infer_type_from_expression(expr, result)
  print *, '  Type:', result%base_type, ' (expected 3 for logical)'
  print *
  
  ! Test another failing case
  expr = "3.14 > 3"
  print *, 'Testing:', trim(expr)
  pos = index(expr, '>')
  print *, '  Position of >:', pos
  call infer_type_from_expression(expr, result)
  print *, '  Type:', result%base_type, ' (expected 3 for logical)'
  print *
  
  ! Test with spaces
  expr = "y<5.0"
  print *, 'Testing without spaces:', trim(expr)
  call infer_type_from_expression(expr, result)
  print *, '  Type:', result%base_type, ' (expected 3 for logical)'
  
end program debug_comparison