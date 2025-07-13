program test_preprocessor_sub
  use preprocessor, only: preprocess_file
  implicit none
  character(len=1024) :: error_msg
  
  call preprocess_file("test_sub.f", "/tmp/test_sub_output.f90", error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Preprocessing successful"
  else
    print *, "Preprocessing failed:", trim(error_msg)
  end if
end program test_preprocessor_sub