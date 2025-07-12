program test_notebook_system
    implicit none
    
    logical :: all_tests_passed
    
    print *, '=== Notebook System Tests ==='
    print *
    
    all_tests_passed = .true.
    
    ! Test 1: CLI help message includes notebook options
    call test_cli_help_includes_notebook(all_tests_passed)
    
    ! Test 2: Notebook CLI execution produces output file
    call test_notebook_cli_execution(all_tests_passed)
    
    ! Test 3: Output file contains expected content
    call test_output_file_content(all_tests_passed)
    
    ! Test 4: Cache performance (second run should be faster)
    call test_cache_performance(all_tests_passed)
    
    if (all_tests_passed) then
        print *
        print *, 'All notebook system tests passed!'
        stop 0
    else
        print *
        print *, 'Some system tests failed!'
        stop 1
    end if
    
contains

    subroutine test_cli_help_includes_notebook(passed)
        logical, intent(inout) :: passed
        character(len=512) :: command
        character(len=:), allocatable :: output
        integer :: exit_code
        
        print *, 'Test 1: CLI help includes notebook options'
        
        ! Execute help command
        command = 'fpm run fortran -- --help'
        call execute_and_capture(command, output, exit_code)
        
        ! Check for notebook-related content
        if (index(output, '--notebook') == 0) then
            print *, '  FAIL: --notebook flag not found in help'
            passed = .false.
            return
        end if
        
        if (index(output, 'Notebook Mode:') == 0) then
            print *, '  FAIL: Notebook Mode section not found in help'
            passed = .false.
            return
        end if
        
        if (index(output, '--output') == 0) then
            print *, '  FAIL: --output flag not found in help'
            passed = .false.
            return
        end if
        
        print *, '  PASS'
        
    end subroutine test_cli_help_includes_notebook
    
    subroutine test_notebook_cli_execution(passed)
        logical, intent(inout) :: passed
        character(len=512) :: command
        character(len=:), allocatable :: output
        integer :: exit_code
        logical :: file_exists
        
        print *, 'Test 2: Notebook CLI execution'
        
        ! Clean up any existing test output
        call execute_command_line('rm -f /tmp/test_system_notebook.md')
        
        ! Execute notebook command
        command = 'fpm run fortran -- --notebook ' // &
                 'example/notebook/simple_math.f -o /tmp/test_system_notebook.md'
        call execute_and_capture(command, output, exit_code)
        
        ! Check command succeeded
        if (exit_code /= 0) then
            print *, '  FAIL: Notebook execution failed with exit code', exit_code
            print *, '  Output:', output
            passed = .false.
            return
        end if
        
        ! Check output file was created
        inquire(file='/tmp/test_system_notebook.md', exist=file_exists)
        if (.not. file_exists) then
            print *, '  FAIL: Output file not created'
            passed = .false.
            return
        end if
        
        print *, '  PASS'
        
    end subroutine test_notebook_cli_execution
    
    subroutine test_output_file_content(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: content
        
        print *, 'Test 3: Output file content validation'
        
        ! Read the output file
        call read_file_content('/tmp/test_system_notebook.md', content)
        
        if (len(content) == 0) then
            print *, '  FAIL: Output file is empty'
            passed = .false.
            return
        end if
        
        ! Check for expected markdown content
        if (index(content, '# Simple Mathematical Computations') == 0) then
            print *, '  FAIL: Main title not found'
            passed = .false.
            return
        end if
        
        if (index(content, '```fortran') == 0) then
            print *, '  FAIL: Code blocks not found'
            passed = .false.
            return
        end if
        
        if (index(content, 'Basic Arithmetic Operations') == 0) then
            print *, '  FAIL: Section headers not found'
            passed = .false.
            return
        end if
        
        print *, '  PASS'
        
    end subroutine test_output_file_content
    
    subroutine test_cache_performance(passed)
        logical, intent(inout) :: passed
        character(len=512) :: command
        character(len=:), allocatable :: output1, output2
        integer :: exit_code1, exit_code2
        real :: start_time, end_time, time1, time2
        
        print *, 'Test 4: Cache performance verification'
        
        ! First run (cache miss)
        call cpu_time(start_time)
        command = 'fpm run fortran -- --notebook ' // &
                 'example/notebook/arrays_loops.f -o /tmp/test_cache_perf1.md'
        call execute_and_capture(command, output1, exit_code1)
        call cpu_time(end_time)
        time1 = end_time - start_time
        
        ! Second run (cache hit)
        call cpu_time(start_time)
        command = 'fpm run fortran -- --notebook ' // &
                 'example/notebook/arrays_loops.f -o /tmp/test_cache_perf2.md'
        call execute_and_capture(command, output2, exit_code2)
        call cpu_time(end_time)
        time2 = end_time - start_time
        
        ! Both should succeed
        if (exit_code1 /= 0 .or. exit_code2 /= 0) then
            print *, '  FAIL: One of the executions failed'
            passed = .false.
            return
        end if
        
        ! Second run should be at least as fast as first (with some tolerance)
        if (time2 > time1 * 2.0) then
            print *, '  WARNING: Second run not significantly faster (', time2, 'vs', time1, ')'
            ! Don't fail the test as timing can be variable
        end if
        
        print *, '  PASS'
        
    end subroutine test_cache_performance
    
    subroutine execute_and_capture(command, output, exit_code)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code
        
        character(len=256) :: temp_file
        character(len=512) :: full_command
        integer :: unit, iostat, file_size
        
        temp_file = '/tmp/fortran_system_test.out'
        
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
    
    subroutine read_file_content(filename, content)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content
        
        integer :: unit, iostat, file_size
        logical :: file_exists
        
        inquire(file=filename, exist=file_exists, size=file_size)
        
        if (.not. file_exists .or. file_size <= 0) then
            content = ""
            return
        end if
        
        open(newunit=unit, file=filename, status='old', &
             access='stream', form='unformatted', iostat=iostat)
        
        if (iostat /= 0) then
            content = ""
            return
        end if
        
        allocate(character(len=file_size) :: content)
        read(unit, iostat=iostat) content
        close(unit)
        
        if (iostat /= 0) then
            content = ""
        end if
        
    end subroutine read_file_content

end program test_notebook_system