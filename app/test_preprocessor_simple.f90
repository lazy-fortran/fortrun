program test_standardizer_simple
    use standardizer, only: standardize_file
    implicit none
    
    character(len=256) :: error_msg
    
    call standardize_file('/tmp/test_standardize.f', '/tmp/test_standardize.f90', error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, 'Error: ', trim(error_msg)
    else
        print *, 'Success - check /tmp/test_standardize.f90'
    end if
end program