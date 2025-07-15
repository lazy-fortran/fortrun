program test_manual_check
    use preprocessor, only: preprocess_file
    implicit none
    
    character(len=256) :: error_msg
    
    call preprocess_file('/tmp/test_func_sig.f', '/tmp/test_func_sig.f90', error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, 'Error: ', trim(error_msg)
    else
        print *, 'Generated /tmp/test_func_sig.f90'
    end if
end program