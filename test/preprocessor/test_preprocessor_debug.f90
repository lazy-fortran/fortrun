program test_preprocessor_debug
  use preprocessor
  implicit none
  
  character(len=256) :: temp_file, output_file, error_msg
  character(len=1024) :: line
  integer :: unit, ios, program_count, line_num
  
  ! Create temporary test file with existing program
  temp_file = 'test_existing_debug.f'
  output_file = 'test_existing_debug.f90'
  open(newunit=unit, file=temp_file, status='replace')
  write(unit, '(a)') "program mytest"
  write(unit, '(a)') "  implicit none"
  write(unit, '(a)') "  print *, 'Already has program'"
  write(unit, '(a)') "end program mytest"
  close(unit)
  
  ! Preprocess
  call preprocess_file(temp_file, output_file, error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'PASS: Existing program preprocessing completed'
  else
    print '(a,a)', 'FAIL: Existing program preprocessing error: ', trim(error_msg)
    stop 1
  end if
  
  ! Check output doesn't have double program statements
  program_count = 0
  line_num = 0
  
  open(newunit=unit, file=output_file, status='old', action='read')
  do
    read(unit, '(a)', iostat=ios) line
    if (ios /= 0) exit
    line_num = line_num + 1
    
    if (index(adjustl(line), 'program ') == 1 .and. &
        index(line, 'end program') == 0) then
      program_count = program_count + 1
      print '(a,i0,a,a)', 'Found program statement #', program_count, ' at line ', trim(adjustl(line))
    end if
  end do
  close(unit)
  
  if (program_count == 1) then
    print '(a)', 'PASS: Existing program not wrapped again'
  else
    print '(a,i0)', 'FAIL: Wrong number of program statements: ', program_count
  end if
  
  ! Show full output
  print '(a)', ''
  print '(a)', 'Full output file contents:'
  open(newunit=unit, file=output_file, status='old', action='read')
  line_num = 0
  do
    read(unit, '(a)', iostat=ios) line
    if (ios /= 0) exit
    line_num = line_num + 1
    print '(i3,a,a)', line_num, ': ', trim(line)
  end do
  close(unit)
  
  ! Cleanup
  call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  
end program test_preprocessor_debug