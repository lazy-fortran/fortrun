program debug_preprocess
  use preprocessor
  implicit none
  
  character(len=256) :: temp_file, output_file, error_msg
  character(len=1024) :: line
  integer :: unit, ios
  
  ! Same as test
  temp_file = 'test_existing_pp.f'
  output_file = 'test_existing_pp.f90'
  
  ! Clean first
  call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  
  open(newunit=unit, file=temp_file, status='replace')
  write(unit, '(a)') "program mytest"
  write(unit, '(a)') "  implicit none"
  write(unit, '(a)') "  print *, 'Already has program'"
  write(unit, '(a)') "end program mytest"
  close(unit)
  
  print '(a)', 'Input file contents:'
  open(newunit=unit, file=temp_file, status='old', action='read')
  do
    read(unit, '(a)', iostat=ios) line
    if (ios /= 0) exit
    print '(a,a)', '  ', trim(line)
  end do
  close(unit)
  
  ! Preprocess
  call preprocess_file(temp_file, output_file, error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Preprocessing completed successfully'
  else
    print '(a,a)', 'Error: ', trim(error_msg)
    stop 1
  end if
  
  print '(a)', ''
  print '(a)', 'Output file contents:'
  open(newunit=unit, file=output_file, status='old', action='read')
  do
    read(unit, '(a)', iostat=ios) line
    if (ios /= 0) exit
    print '(a,a)', '  ', trim(line)
  end do
  close(unit)
  
  ! Check file size
  print '(a)', ''
  call execute_command_line('ls -la ' // trim(output_file))
  
  ! Cleanup
  call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  
end program debug_preprocess