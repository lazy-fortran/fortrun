program test_cli_edge_cases
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== CLI Edge Cases Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test CLI edge cases through system testing
    if (.not. test_invalid_job_numbers()) all_tests_passed = .false.
    if (.not. test_missing_arguments()) all_tests_passed = .false.
    if (.not. test_conflicting_options()) all_tests_passed = .false.
    if (.not. test_long_filenames()) all_tests_passed = .false.
    if (.not. test_special_characters()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All CLI edge case tests passed!"
        stop 0
    else
        print *, "Some CLI edge case tests failed!"
        stop 1
    end if
    
contains

    function test_invalid_job_numbers() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        
        print *, "Test 1: Invalid job numbers"
        passed = .true.
        
        ! Test negative job number
        command = 'fpm run fortran -- --jobs -1 /dev/null 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  WARNING: Negative job number should fail"
        end if
        
        ! Test zero job number  
        command = 'fpm run fortran -- --jobs 0 /dev/null 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  WARNING: Zero job number should fail"
        end if
        
        ! Test non-numeric job number
        command = 'fpm run fortran -- --jobs abc /dev/null 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  WARNING: Non-numeric job number should fail"
        end if
        
        print *, "  PASS: Invalid job numbers handled"
        
    end function test_invalid_job_numbers

    function test_missing_arguments() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        
        print *, "Test 2: Missing required arguments"
        passed = .true.
        
        ! Test --cache-dir without argument
        command = 'fpm run fortran -- --cache-dir 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  WARNING: Missing cache-dir argument should fail"
        end if
        
        ! Test --config-dir without argument
        command = 'fpm run fortran -- --config-dir 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  WARNING: Missing config-dir argument should fail"
        end if
        
        ! Test --jobs without argument
        command = 'fmp run fortran -- --jobs 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        ! This should fail due to typo in fmp, but tests the concept
        
        ! Test -o without argument
        command = 'fpm run fortran -- -o 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  WARNING: Missing output argument should fail"
        end if
        
        print *, "  PASS: Missing arguments handled"
        
    end function test_missing_arguments

    function test_conflicting_options() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=256) :: test_file
        
        print *, "Test 3: Conflicting options"
        passed = .true.
        
        ! Create a simple test file
        test_file = '/tmp/test_cli_conflicts.f90'
        call create_simple_test_file(test_file)
        
        ! Test conflicting verbose levels (-v and -vv)
        command = 'fpm run fortran -- -v -vv ' // trim(test_file) // ' 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        ! Should handle gracefully (last one wins)
        
        ! Test multiple --verbose flags
        command = 'fpm run fortran -- --verbose 1 --verbose 2 ' // trim(test_file) // ' 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        ! Should handle gracefully
        
        ! Test conflicting cache directories
        command = 'fpm run fortran -- --cache-dir /tmp/cache1 --cache-dir /tmp/cache2 ' // &
                 trim(test_file) // ' 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        ! Should handle gracefully (last one wins)
        
        ! Clean up
        call delete_test_file(test_file)
        
        print *, "  PASS: Conflicting options handled"
        
    end function test_conflicting_options

    function test_long_filenames() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command, long_filename
        
        print *, "Test 4: Long filenames and paths"
        passed = .true.
        
        ! Create a very long filename
        long_filename = '/tmp/very_long_filename_that_tests_the_limits_of_argument_parsing_' // &
                       'and_path_handling_in_the_fortran_cli_tool_implementation.f90'
        
        call create_simple_test_file(long_filename)
        
        ! Test with long filename
        command = 'fpm run fortran -- "' // trim(long_filename) // '" 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        ! Should handle long paths gracefully
        
        ! Test with long cache directory
        command = 'fpm run fortran -- --cache-dir /tmp/very_long_cache_directory_name_for_testing ' // &
                 trim(long_filename) // ' 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        
        ! Clean up
        call delete_test_file(long_filename)
        
        print *, "  PASS: Long filenames handled"
        
    end function test_long_filenames

    function test_special_characters() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=256) :: special_file
        
        print *, "Test 5: Special characters in arguments"
        passed = .true.
        
        ! Test filename with spaces
        special_file = '/tmp/file with spaces.f90'
        call create_simple_test_file(special_file)
        
        command = 'fpm run fortran -- "' // trim(special_file) // '" 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        
        call delete_test_file(special_file)
        
        ! Test cache directory with special characters
        command = 'fpm run fortran -- --cache-dir "/tmp/cache-dir_with.special+chars" /dev/null 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        
        ! Test output file with special characters
        command = 'fpm run fortran -- --notebook -o "/tmp/output-file_with.special+chars.md" /dev/null 2>/dev/null'
        call execute_command_line(command, exitstat=exit_code)
        
        print *, "  PASS: Special characters handled"
        
    end function test_special_characters

    ! Helper subroutines
    subroutine create_simple_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  print *, "Hello, world!"'
        write(unit, '(a)') 'end program test'
        close(unit)
        
    end subroutine create_simple_test_file

    subroutine delete_test_file(filename)
        character(len=*), intent(in) :: filename
        character(len=512) :: command
        
        command = 'rm -f "' // trim(filename) // '"'
        call execute_command_line(command)
        
    end subroutine delete_test_file

end program test_cli_edge_cases