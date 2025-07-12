program debug_square_type
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test simple untyped function
  open(10, file='test_square.f', status='replace')
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '    square = val * val'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Testing untyped function declaration:"
  call preprocess_file('test_square.f', 'test_square_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n test_square_out.f90')
    print *, ""
    print *, "Looking for square declaration:"
    call system('grep ":: square" test_square_out.f90 || echo "  NOT FOUND"')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_square.f test_square_out.f90')
  
end program debug_square_type