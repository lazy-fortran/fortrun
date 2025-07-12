program debug_preprocess_direct
    use preprocessor, only: preprocess_file
    implicit none
    
    character(len=256) :: error_msg
    integer :: unit, ios
    
    ! Create the exact test file content
    open(newunit=unit, file='debug_step1_test.f', status='replace', action='write', iostat=ios)
    if (ios == 0) then
        write(unit, '(A)') 'y = compute(3, 4)'
        write(unit, '(A)') ''
        write(unit, '(A)') 'integer function compute(a, b)'
        write(unit, '(A)') '  integer :: a, b'
        write(unit, '(A)') '  compute = a + b'
        write(unit, '(A)') 'end function'
        close(unit)
    end if
    
    call preprocess_file('debug_step1_test.f', 'debug_step1_test.f90', error_msg)
    
    print *, 'Error message length:', len_trim(error_msg)
    if (len_trim(error_msg) /= 0) then
        print *, 'ERROR MESSAGE: "', trim(error_msg), '"'
    else
        print *, 'No error message - SUCCESS!'
    end if
    
    ! Check output
    print *, 'Checking for required strings...'
    print *, 'Looking for "integer(4), intent(in) :: a, b":', check_contains('debug_step1_test.f90', 'integer(4), intent(in) :: a, b')
    print *, 'Looking for "integer(4) :: y":', check_contains('debug_step1_test.f90', 'integer(4) :: y')
    
contains

    function check_contains(filename, text) result(found)
        character(len=*), intent(in) :: filename, text
        logical :: found
        character(len=1024) :: line
        integer :: unit, ios
        
        found = .false.
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, trim(text)) > 0) then
                    found = .true.
                    exit
                end if
            end do
            close(unit)
        end if
    end function
    
end program