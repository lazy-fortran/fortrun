program debug_math_fix
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test the actual math.f file
  print *, "Testing math.f directly:"
  call preprocess_file('example/preprocessor/math.f', 'math_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n math_out.f90 | head -40')
    print *, ""
    print *, "Checking end function lines:"
    call system('grep -n "end function" math_out.f90')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f math_out.f90')
  
end program debug_math_fix