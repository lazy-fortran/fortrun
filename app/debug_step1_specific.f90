program debug_step1_specific
    use preprocessor, only: preprocess_file
    implicit none
    
    character(len=256) :: input_file, output_file, error_msg
    logical :: success1, success2
    
    input_file = 'test_intent_debug.f'
    output_file = 'test_intent_debug.f90'
    
    ! Create test file - exact same as test
    call create_test_file(input_file, &
        'y = compute(3, 4)' // new_line('a') // &
        '' // new_line('a') // &
        'integer function compute(a, b)' // new_line('a') // &
        '  integer :: a, b' // new_line('a') // &
        '  compute = a + b' // new_line('a') // &
        'end function')
    
    ! Preprocess
    call preprocess_file(input_file, output_file, error_msg)
    
    if (len_trim(error_msg) /= 0) then
        print *, 'Error:', trim(error_msg)
        stop 1
    end if
    
    ! Check both conditions
    success1 = check_output_contains(output_file, 'integer(4), intent(in) :: a, b')
    success2 = check_output_contains(output_file, 'integer(4) :: y')
    
    print *, 'Looking for "integer(4), intent(in) :: a, b": ', success1
    print *, 'Looking for "integer(4) :: y": ', success2
    print *, 'Overall success: ', success1 .and. success2
    
    ! Show full output for debugging
    print *, 'Full output:'
    call system('cat -n ' // trim(output_file))
    
    ! Cleanup
    call cleanup_files(input_file, output_file)
    
contains

    subroutine create_test_file(filename, content)
        character(len=*), intent(in) :: filename, content
        integer :: unit, ios
        
        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write(unit, '(A)') content
            close(unit)
        end if
    end subroutine
    
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
                if (index(line, trim(expected_text)) > 0) then
                    found = .true.
                    exit
                end if
            end do
            close(unit)
        end if
    end function
    
    subroutine cleanup_files(file1, file2)
        character(len=*), intent(in) :: file1, file2
        integer :: unit, ios
        
        open(newunit=unit, file=file1, status='old', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
        end if
        
        open(newunit=unit, file=file2, status='old', iostat=ios) 
        if (ios == 0) then
            close(unit, status='delete')
        end if
    end subroutine

end program debug_step1_specific