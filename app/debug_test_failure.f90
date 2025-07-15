program debug_test_failure
    use preprocessor, only: preprocess_file
    implicit none
    
    character(len=256) :: error_msg
    logical :: found
    
    ! Generate the file
    call preprocess_file('/tmp/test_func_sig.f', '/tmp/test_func_sig.f90', error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, 'Error: ', trim(error_msg)
        stop
    end if
    
    ! Check for the expected text
    found = check_output_contains('/tmp/test_func_sig.f90', 'real(8) function compute(x)')
    print *, 'Found function signature:', found
    
    found = check_output_contains('/tmp/test_func_sig.f90', 'real(8), intent(in) :: x')
    print *, 'Found parameter declaration:', found
    
contains

    function check_output_contains(filename, expected_text) result(found)
        character(len=*), intent(in) :: filename, expected_text
        logical :: found
        character(len=1024) :: line
        integer :: unit, ios
        
        found = .false.
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                
                ! Skip debug output and other noise
                if (index(line, 'DEBUG:') > 0 .or. &
                    index(line, '!') == 1) cycle
                
                print *, 'Checking line: "', trim(line), '"'
                print *, 'Looking for: "', trim(expected_text), '"'
                if (index(line, trim(expected_text)) > 0) then
                    print *, 'FOUND!'
                    found = .true.
                    exit
                end if
            end do
            close(unit)
        end if
    end function

end program