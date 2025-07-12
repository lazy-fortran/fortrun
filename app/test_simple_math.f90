program test_simple_math
  use preprocessor
  implicit none
  
  character(len=256) :: error_msg
  
  call preprocess_file('example/notebook/simple_math.f', 'test_simple_math_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Generated variables:'
    call execute_command_line('grep -A20 "Auto-generated" test_simple_math_out.f90 | grep "::"')
    call execute_command_line('rm -f test_simple_math_out.f90')
  else
    print '(a,a)', 'Error: ', trim(error_msg)
  end if
  
end program test_simple_math