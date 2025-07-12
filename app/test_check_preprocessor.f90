program test_check_preprocessor
  use preprocessor
  implicit none
  
  character(len=256) :: temp_file, output_file, error_msg
  character(len=1024) :: line
  integer :: unit, ios
  
  ! Create the same test file as in test_function_preprocessing
  temp_file = 'test_function_check.f'
  output_file = 'test_function_check.f90'
  open(newunit=unit, file=temp_file, status='replace')
  write(unit, '(a)') "x = add(1.0, 2.0)"
  write(unit, '(a)') "print *, x"
  write(unit, '(a)') ""
  write(unit, '(a)') "real function add(a, b)"
  write(unit, '(a)') "  real :: a, b"
  write(unit, '(a)') "  add = a + b"
  write(unit, '(a)') "end function add"
  close(unit)
  
  ! Preprocess
  call preprocess_file(temp_file, output_file, error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Preprocessing completed successfully'
    
    ! Print the output file
    print '(a)', 'Output file contents:'
    open(newunit=unit, file=output_file, status='old', action='read')
    do
      read(unit, '(a)', iostat=ios) line
      if (ios /= 0) exit
      print '(a)', trim(line)
    end do
    close(unit)
  else
    print '(a,a)', 'Error: ', trim(error_msg)
  end if
  
  ! Cleanup
  call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  
end program test_check_preprocessor