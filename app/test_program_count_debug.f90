program test_program_count_debug
  implicit none
  
  character(len=1024) :: line
  integer :: program_count
  
  ! Test the exact pattern used in test
  program_count = 0
  
  ! Test various lines
  line = "program mytest"
  if (index(adjustl(line), 'program ') == 1 .and. &
      index(line, 'end program') == 0) then
    program_count = program_count + 1
    print '(a,a,a,i0)', 'Line "', trim(line), '" matches. Count = ', program_count
  end if
  
  line = "  program another"
  if (index(adjustl(line), 'program ') == 1 .and. &
      index(line, 'end program') == 0) then
    program_count = program_count + 1
    print '(a,a,a,i0)', 'Line "', trim(line), '" matches. Count = ', program_count
  end if
  
  line = "end program mytest"
  if (index(adjustl(line), 'program ') == 1 .and. &
      index(line, 'end program') == 0) then
    program_count = program_count + 1
    print '(a,a,a,i0)', 'Line "', trim(line), '" matches. Count = ', program_count
  else
    print '(a,a,a)', 'Line "', trim(line), '" does not match (correct)'
  end if
  
  line = "program main"
  if (index(adjustl(line), 'program ') == 1 .and. &
      index(line, 'end program') == 0) then
    program_count = program_count + 1
    print '(a,a,a,i0)', 'Line "', trim(line), '" matches. Count = ', program_count
  end if
  
  print '(a,i0)', 'Final count: ', program_count
  
end program test_program_count_debug