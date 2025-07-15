program test_preprocessor_simple
    use preprocessor, only: preprocess_file
    implicit none
    
    character(len=256) :: error_msg
    
    call preprocess_file('/tmp/test_preprocess.f', '/tmp/test_preprocess.f90', error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, 'Error: ', trim(error_msg)
    else
        print *, 'Success - check /tmp/test_preprocess.f90'
    end if
end program