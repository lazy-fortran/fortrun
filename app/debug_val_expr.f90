program debug_val_expr
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Original failing case
  print *, "Original case: square = val * val"
  open(10, file='test_orig.f', status='replace')
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '  square = val * val'
  write(10, '(a)') 'end function'
  close(10)
  
  call preprocess_file('test_orig.f', 'test_orig_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    print *, "Declarations found:"
    call system('grep ":: " test_orig_out.f90 | grep -v "Auto-generated"')
  end if
  
  ! Try with a number
  print *, ""
  print *, "With number: square = val * 2.0"
  open(10, file='test_num.f', status='replace')
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '  square = val * 2.0'
  write(10, '(a)') 'end function'
  close(10)
  
  call preprocess_file('test_num.f', 'test_num_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    print *, "Declarations found:"
    call system('grep ":: " test_num_out.f90 | grep -v "Auto-generated"')
  end if
  
  ! Try with just val
  print *, ""
  print *, "Just val: square = val"
  open(10, file='test_val.f', status='replace')
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '  square = val'
  write(10, '(a)') 'end function'
  close(10)
  
  call preprocess_file('test_val.f', 'test_val_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    print *, "Declarations found:"
    call system('grep ":: " test_val_out.f90 | grep -v "Auto-generated"')
  end if
  
  call system('rm -f test_*.f test_*.f90')
  
end program debug_val_expr