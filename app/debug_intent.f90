program debug_intent
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: input_file, output_file, error_msg
  
  input_file = '/tmp/test_explicit_param.f'
  output_file = '/tmp/test_explicit_param_output.f90'
  
  call preprocess_file(input_file, output_file, error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, 'Preprocessing successful. Output written to: ', trim(output_file)
  else
    print *, 'Error during preprocessing: ', trim(error_msg)
  end if
  
end program debug_intent