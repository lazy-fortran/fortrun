program test_file_isolation
  implicit none
  
  integer :: unit
  
  ! Test 1: Write to file1
  open(newunit=unit, file='file1.txt', status='replace')
  write(unit, '(a)') 'Content from test 1'
  close(unit)
  
  ! Test 2: Write to file2
  open(newunit=unit, file='file2.txt', status='replace')
  write(unit, '(a)') 'Content from test 2'
  close(unit)
  
  ! Test 3: Write to file3
  open(newunit=unit, file='file3.txt', status='replace')
  write(unit, '(a)') 'Content from test 3'
  close(unit)
  
  ! Check contents
  print '(a)', 'file1.txt:'
  call execute_command_line('cat file1.txt')
  
  print '(a)', 'file2.txt:'
  call execute_command_line('cat file2.txt')
  
  print '(a)', 'file3.txt:'
  call execute_command_line('cat file3.txt')
  
  ! Cleanup
  call execute_command_line('rm -f file1.txt file2.txt file3.txt')
  
end program test_file_isolation