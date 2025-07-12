program debug_preprocessor
  use preprocessor
  implicit none
  
  character(len=256) :: input_file, output_file, error_msg
  
  input_file = 'explore/test_scope_simple.f'
  output_file = 'explore/test_scope_debug.f90'
  
  print *, "=== DEBUG: Preprocessing", trim(input_file), "==="
  
  call preprocess_file(input_file, output_file, error_msg)
  
  if (len_trim(error_msg) > 0) then
    print *, "ERROR:", trim(error_msg)
  else
    print *, "SUCCESS: Output written to", trim(output_file)
  end if
  
end program debug_preprocessor