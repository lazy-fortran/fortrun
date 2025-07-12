program test_control_flow
  use preprocessor
  implicit none
  
  character(len=256) :: error_msg
  
  call preprocess_file('example/notebook/control_flow_simple.f', 'test_control_flow_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Generated variables:'
    call execute_command_line('grep -A50 "Auto-generated" test_control_flow_out.f90 | grep "::" | head -20')
    call execute_command_line('rm -f test_control_flow_out.f90')
  else
    print '(a,a)', 'Error: ', trim(error_msg)
  end if
  
end program test_control_flow