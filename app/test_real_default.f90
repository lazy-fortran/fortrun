program test_real_default
  use preprocessor
  implicit none
  
  character(len=256) :: error_msg
  
  call preprocess_file('example/precision/real_default_test.f', 'test_real_default_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Generated code:'
    print '(a)', '==============='
    call execute_command_line('cat test_real_default_out.f90')
    call execute_command_line('rm -f test_real_default_out.f90')
  else
    print '(a,a)', 'Error: ', trim(error_msg)
  end if
  
end program test_real_default