program test_func_debug
  use preprocessor
  implicit none
  
  character(len=256) :: input_file, output_file, error_msg
  
  ! Same test as in test
  input_file = 'test_func_param.f'
  output_file = 'test_func_param.f90'
  
  call create_test_file(input_file, &
      'x = 5.0' // new_line('a') // &
      'y = square(x)' // new_line('a') // &
      '' // new_line('a') // &
      'function square(val)' // new_line('a') // &
      '    square = val * val' // new_line('a') // &
      'end function')
  
  call preprocess_file(input_file, output_file, error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Generated output:'
    call execute_command_line('cat ' // trim(output_file))
    
    print '(a)', ''
    print '(a)', 'Checking for: real(8), intent(in) :: val'
    call execute_command_line('grep "real(8), intent(in) :: val" ' // trim(output_file) // ' && echo "FOUND" || echo "NOT FOUND"')
    
    print '(a)', ''
    print '(a)', 'Checking for: real(8) :: square'
    call execute_command_line('grep "real(8) :: square" ' // trim(output_file) // ' && echo "FOUND" || echo "NOT FOUND"')
  else
    print '(a,a)', 'Error: ', trim(error_msg)
  end if
  
  call execute_command_line('rm -f ' // trim(input_file) // ' ' // trim(output_file))
  
contains

  subroutine create_test_file(filename, content)
    character(len=*), intent(in) :: filename, content
    integer :: unit
    
    open(newunit=unit, file=filename, status='replace')
    write(unit, '(A)') content
    close(unit)
  end subroutine

end program test_func_debug