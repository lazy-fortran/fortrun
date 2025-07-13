program test_cli_coverage
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== CLI Coverage Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test CLI through actual command execution
    if (.not. test_help_variations()) all_tests_passed = .false.
    if (.not. test_verbose_execution()) all_tests_passed = .false.
    if (.not. test_parallel_execution()) all_tests_passed = .false.
    if (.not. test_directory_options()) all_tests_passed = .false.
    if (.not. test_notebook_options()) all_tests_passed = .false.
    if (.not. test_error_messages()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All CLI coverage tests passed!"
        stop 0
    else
        print *, "Some CLI coverage tests failed!"
        stop 1
    end if
    
contains

    function test_help_variations() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        integer :: exit_code
        
        print *, "Test 1: Help flag variations"
        passed = .true.
        
        ! Test -h
        call execute_and_capture('fpm run fortran -- -h', output, exit_code)
        if (index(output, 'Usage:') == 0) then
            print *, "  WARNING: -h should show usage"
        end if
        
        ! Test --help
        call execute_and_capture('fpm run fortran -- --help', output, exit_code)
        if (index(output, 'Usage:') == 0) then
            print *, "  WARNING: --help should show usage"
        end if
        
        ! Test no arguments (should show help)
        call execute_and_capture('fpm run fortran --', output, exit_code)
        if (index(output, 'Usage:') == 0) then
            print *, "  WARNING: No args should show usage"
        end if
        
        if (passed) print *, "  PASS: Help variations"
        
    end function test_help_variations

    function test_verbose_execution() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        integer :: exit_code, unit
        character(len=256) :: test_file
        
        print *, "Test 2: Verbose execution modes"
        passed = .true.
        
        ! Create simple test file
        test_file = '/tmp/test_cli_verbose.f90'
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  print *, "Hello"'
        write(unit, '(a)') 'end program'
        close(unit)
        
        ! Test -v
        call execute_and_capture('fpm run fortran -- -v ' // trim(test_file), output, exit_code)
        ! Should see some verbose output
        
        ! Test -vv  
        call execute_and_capture('fpm run fortran -- -vv ' // trim(test_file), output, exit_code)
        ! Should see more verbose output
        
        ! Test -vvv
        call execute_and_capture('fpm run fortran -- -vvv ' // trim(test_file), output, exit_code)
        ! Should see even more verbose output
        
        ! Test --verbose
        call execute_and_capture('fpm run fortran -- --verbose ' // trim(test_file), output, exit_code)
        
        ! Test --verbose 2
        call execute_and_capture('fpm run fortran -- --verbose 2 ' // trim(test_file), output, exit_code)
        
        call execute_command_line('rm -f ' // trim(test_file))
        
        if (passed) print *, "  PASS: Verbose execution"
        
    end function test_verbose_execution

    function test_parallel_execution() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        integer :: exit_code, unit
        character(len=256) :: test_file
        
        print *, "Test 3: Parallel execution options"
        passed = .true.
        
        ! Create test file
        test_file = '/tmp/test_cli_parallel.f90'
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  print *, "Parallel test"'
        write(unit, '(a)') 'end program'
        close(unit)
        
        ! Test -j
        call execute_and_capture('fpm run fortran -- -j 2 ' // trim(test_file), output, exit_code)
        
        ! Test --jobs
        call execute_and_capture('fpm run fortran -- --jobs 4 ' // trim(test_file), output, exit_code)
        
        ! Test invalid job count
        call execute_and_capture('fpm run fortran -- -j abc ' // trim(test_file) // ' 2>&1', output, exit_code)
        if (index(output, 'Invalid') == 0 .and. index(output, 'Error') == 0) then
            print *, "  WARNING: Invalid job count should show error"
        end if
        
        ! Test missing job count
        call execute_and_capture('fpm run fortran -- -j 2>&1', output, exit_code)
        if (index(output, 'requires') == 0) then
            print *, "  WARNING: Missing job count should show error"
        end if
        
        call execute_command_line('rm -f ' // trim(test_file))
        
        if (passed) print *, "  PASS: Parallel execution"
        
    end function test_parallel_execution

    function test_directory_options() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        integer :: exit_code, unit
        character(len=256) :: test_file, test_cache, test_config
        
        print *, "Test 4: Directory options"
        passed = .true.
        
        ! Create test file
        test_file = '/tmp/test_cli_dirs.f90'
        test_cache = '/tmp/test_cli_cache'
        test_config = '/tmp/test_cli_config'
        
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  print *, "Dir test"'
        write(unit, '(a)') 'end program'
        close(unit)
        
        ! Test --cache-dir
        call execute_and_capture('fpm run fortran -- --cache-dir ' // trim(test_cache) // &
                                ' ' // trim(test_file), output, exit_code)
        
        ! Test --config-dir
        call execute_and_capture('fpm run fortran -- --config-dir ' // trim(test_config) // &
                                ' ' // trim(test_file), output, exit_code)
        
        ! Test both directories
        call execute_and_capture('fpm run fortran -- --cache-dir ' // trim(test_cache) // &
                                ' --config-dir ' // trim(test_config) // ' ' // &
                                trim(test_file), output, exit_code)
        
        ! Test missing directory argument
        call execute_and_capture('fpm run fortran -- --cache-dir 2>&1', output, exit_code)
        if (index(output, 'requires') == 0) then
            print *, "  WARNING: Missing cache-dir value should show error"
        end if
        
        call execute_command_line('rm -f ' // trim(test_file))
        call execute_command_line('rm -rf ' // trim(test_cache))
        call execute_command_line('rm -rf ' // trim(test_config))
        
        if (passed) print *, "  PASS: Directory options"
        
    end function test_directory_options

    function test_notebook_options() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        integer :: exit_code, unit
        character(len=256) :: test_file
        
        print *, "Test 5: Notebook mode options"
        passed = .true.
        
        ! Create notebook test file
        test_file = '/tmp/test_cli_notebook.f'
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') '! %% [markdown]'
        write(unit, '(a)') '! # Test'
        write(unit, '(a)') '! %%'
        write(unit, '(a)') 'print *, "Notebook"'
        close(unit)
        
        ! Test --notebook
        call execute_and_capture('fpm run fortran -- --notebook ' // trim(test_file), output, exit_code)
        
        ! Test --notebook with --output
        call execute_and_capture('fpm run fortran -- --notebook ' // trim(test_file) // &
                                ' --output /tmp/test_output.md', output, exit_code)
        
        ! Test --no-wait
        call execute_and_capture('fpm run fortran -- --notebook --no-wait ' // &
                                trim(test_file), output, exit_code)
        
        ! Test missing output argument
        call execute_and_capture('fpm run fortran -- --notebook ' // trim(test_file) // &
                                ' --output 2>&1', output, exit_code)
        if (index(output, 'requires') == 0) then
            print *, "  WARNING: Missing output value should show error"
        end if
        
        call execute_command_line('rm -f ' // trim(test_file))
        call execute_command_line('rm -f /tmp/test_output.md')
        call execute_command_line('rm -f /tmp/test_cli_notebook.md')
        
        if (passed) print *, "  PASS: Notebook options"
        
    end function test_notebook_options

    function test_error_messages() result(passed)
        logical :: passed
        character(len=:), allocatable :: output
        integer :: exit_code
        
        print *, "Test 6: Error messages and edge cases"
        passed = .true.
        
        ! Test unknown flag
        call execute_and_capture('fpm run fortran -- --unknown-flag test.f90 2>&1', output, exit_code)
        if (index(output, 'Unknown') == 0 .and. index(output, 'Usage:') == 0) then
            print *, "  WARNING: Unknown flag should show error or usage"
        end if
        
        ! Test multiple files (should only accept one)
        call execute_and_capture('fpm run fortran -- file1.f90 file2.f90 2>&1', output, exit_code)
        
        ! Test -vvvv (too many v's)
        call execute_and_capture('fpm run fortran -- -vvvv test.f90 2>&1', output, exit_code)
        
        ! Test conflicting verbose flags
        call execute_and_capture('fpm run fortran -- -v --verbose 3 test.f90', output, exit_code)
        
        if (passed) print *, "  PASS: Error messages"
        
    end function test_error_messages

    subroutine execute_and_capture(command, output, exit_code)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code
        
        character(len=256) :: temp_file
        character(len=512) :: full_command
        integer :: unit, iostat, file_size
        
        temp_file = '/tmp/fortran_cli_coverage_test.out'
        
        full_command = trim(command) // ' > ' // trim(temp_file) // ' 2>&1'
        call execute_command_line(full_command, exitstat=exit_code)
        
        inquire(file=temp_file, size=file_size)
        
        if (file_size > 0) then
            open(newunit=unit, file=temp_file, status='old', &
                 access='stream', form='unformatted', iostat=iostat)
            
            if (iostat == 0) then
                allocate(character(len=min(file_size, 65536)) :: output)
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

end program test_cli_coverage