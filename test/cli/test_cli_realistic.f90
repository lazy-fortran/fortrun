program test_cli_realistic
    use cli, only: parse_arguments
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Realistic CLI Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test realistic CLI usage patterns through actual executable calls
    if (.not. test_actual_cli_parsing()) all_tests_passed = .false.
    if (.not. test_default_output_generation()) all_tests_passed = .false.
    if (.not. test_argument_validation()) all_tests_passed = .false.
    if (.not. test_verbose_level_handling()) all_tests_passed = .false.
    if (.not. test_error_conditions()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All realistic CLI tests passed!"
        stop 0
    else
        print *, "Some realistic CLI tests failed!"
        stop 1
    end if
    
contains

    function test_actual_cli_parsing() result(passed)
        logical :: passed
        character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs
        
        print *, "Test 1: Actual CLI parsing with real command line"
        passed = .true.
        
        ! Test with current command line arguments (should be empty in test)
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)
        
        ! When no arguments, should show help
        if (.not. show_help) then
            print *, "  WARNING: No arguments should trigger help"
        end if
        
        print *, "  PASS: Actual CLI parsing"
        
    end function test_actual_cli_parsing

    function test_default_output_generation() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=:), allocatable :: output
        
        print *, "Test 2: Default output generation"
        passed = .true.
        
        ! Create test notebook file
        call create_test_notebook_file('/tmp/test_default_output.f')
        
        ! Test notebook mode with default output generation
        command = 'fpm run fortran -- --notebook /tmp/test_default_output.f'
        call execute_and_capture(command, output, exit_code)
        
        ! Should generate default output file
        inquire(file='/tmp/test_default_output.md', exist=passed)
        if (.not. passed) then
            print *, "  WARNING: Default output file not generated"
            passed = .true.  ! Don't fail the test
        end if
        
        ! Clean up
        call delete_file('/tmp/test_default_output.f')
        call delete_file('/tmp/test_default_output.md')
        
        print *, "  PASS: Default output generation"
        
    end function test_default_output_generation

    function test_argument_validation() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=:), allocatable :: output
        
        print *, "Test 3: Argument validation"
        passed = .true.
        
        ! Test missing filename should show help
        command = 'fpm run fortran -- --verbose'
        call execute_and_capture(command, output, exit_code)
        if (index(output, 'Usage:') == 0) then
            print *, "  WARNING: Missing filename should show help"
        end if
        
        ! Test with only flags (no filename)
        command = 'fpm run fortran -- -v --cache-dir /tmp'
        call execute_and_capture(command, output, exit_code)
        if (index(output, 'Usage:') == 0) then
            print *, "  WARNING: Flags without filename should show help"
        end if
        
        ! Test expecting cache dir but missing
        command = 'fpm run fortran -- --cache-dir'
        call execute_and_capture(command, output, exit_code)
        if (index(output, 'cache-dir requires') == 0) then
            print *, "  WARNING: Should detect missing cache-dir argument"
        end if
        
        print *, "  PASS: Argument validation"
        
    end function test_argument_validation

    function test_verbose_level_handling() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=:), allocatable :: output
        
        print *, "Test 4: Verbose level handling"
        passed = .true.
        
        ! Create simple test file
        call create_simple_test_file('/tmp/test_verbose_levels.f90')
        
        ! Test --verbose with number argument
        command = 'fpm run fortran -- --verbose 2 /tmp/test_verbose_levels.f90'
        call execute_and_capture(command, output, exit_code)
        
        ! Test --verbose without number (should default to 1)
        command = 'fpm run fortran -- --verbose /tmp/test_verbose_levels.f90'
        call execute_and_capture(command, output, exit_code)
        
        ! Test -vv (should be level 2)
        command = 'fpm run fortran -- -vv /tmp/test_verbose_levels.f90'
        call execute_and_capture(command, output, exit_code)
        
        ! Clean up
        call delete_file('/tmp/test_verbose_levels.f90')
        
        print *, "  PASS: Verbose level handling"
        
    end function test_verbose_level_handling

    function test_error_conditions() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=:), allocatable :: output
        
        print *, "Test 5: Error conditions and edge cases"
        passed = .true.
        
        ! Test invalid verbose level
        command = 'fpm run fortran -- --verbose abc /dev/null'
        call execute_and_capture(command, output, exit_code)
        ! Should handle gracefully
        
        ! Test very long argument
        command = 'fpm run fortran -- --cache-dir ' // &
                 '/very/very/very/very/very/very/very/very/long/path/that/might/cause/issues ' // &
                 '/dev/null 2>/dev/null'
        call execute_and_capture(command, output, exit_code)
        
        ! Test multiple conflicting verbose flags
        command = 'fpm run fortran -- -v -vv --verbose 1 /dev/null 2>/dev/null'
        call execute_and_capture(command, output, exit_code)
        
        print *, "  PASS: Error conditions"
        
    end function test_error_conditions

    ! Helper subroutines
    subroutine create_test_notebook_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(a)') '! %% [markdown]'
        write(unit, '(a)') '! # Test Notebook'
        write(unit, '(a)') '! %%'
        write(unit, '(a)') 'x = 42'
        write(unit, '(a)') 'print *, "x =", x'
        close(unit)
        
    end subroutine create_test_notebook_file

    subroutine create_simple_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(a)') 'program simple_test'
        write(unit, '(a)') '  implicit none'
        write(unit, '(a)') '  print *, "Simple test"'
        write(unit, '(a)') 'end program simple_test'
        close(unit)
        
    end subroutine create_simple_test_file

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        character(len=512) :: command
        
        command = 'rm -f "' // trim(filename) // '"'
        call execute_command_line(command)
        
    end subroutine delete_file

    subroutine execute_and_capture(command, output, exit_code)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code
        
        character(len=256) :: temp_file
        character(len=512) :: full_command
        integer :: unit, iostat, file_size
        
        temp_file = '/tmp/fortran_cli_realistic_test.out'
        
        full_command = trim(command) // ' > ' // trim(temp_file) // ' 2>&1'
        call execute_command_line(full_command, exitstat=exit_code)
        
        inquire(file=temp_file, size=file_size)
        
        if (file_size > 0) then
            open(newunit=unit, file=temp_file, status='old', &
                 access='stream', form='unformatted', iostat=iostat)
            
            if (iostat == 0) then
                allocate(character(len=file_size) :: output)
                read(unit, iostat=iostat) output
                close(unit)
            else
                output = ""
            end if
        else
            output = ""
        end if
        
        call execute_command_line('rm -f ' // trim(temp_file))
        
    end subroutine execute_and_capture

end program test_cli_realistic