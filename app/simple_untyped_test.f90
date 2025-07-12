program simple_untyped_test
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  open(10, file='simple_func.f', status='replace')
  write(10, '(a)') 'function add(a, b)'
  write(10, '(a)') '  add = a + b' 
  write(10, '(a)') 'end function'
  close(10)
  
  call preprocess_file('simple_func.f', 'simple_func_output.f90', error_msg)
  
  if (len_trim(error_msg) > 0) then
    print *, "Error: ", trim(error_msg)
  else
    print *, "Preprocessed output:"
    call system('cat -n simple_func_output.f90')
    print *, ""
    print *, "Checking for 'add' variable declaration..."
    call system('grep ":: add" simple_func_output.f90 || echo "NOT FOUND: add declaration"')
  end if
  
  call system('rm -f simple_func.f simple_func_output.f90')
  
end program simple_untyped_test