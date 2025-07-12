program test_calc_preprocess
  use preprocessor
  implicit none
  
  character(len=256) :: error_msg
  
  call preprocess_file('test_x_only.f', 'test_x_only_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Success!'
    call execute_command_line('cat test_x_only_out.f90')
  else
    print '(a,a)', 'Error: ', trim(error_msg)
  end if
  
end program test_calc_preprocess