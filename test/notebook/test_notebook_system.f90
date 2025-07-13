program test_notebook_system
    implicit none
    
    logical :: all_tests_passed
    character(len=256) :: test_file, output_file, cmd
    integer :: unit, iostat, exit_code
    character(len=:), allocatable :: output_content
    
    all_tests_passed = .true.
    
    print *, "=== Notebook System Tests ==="
    print *
    
    ! Test 1: Simple notebook execution
    call test_simple_notebook_system()
    
    ! Test 2: Notebook with custom output
    call test_notebook_custom_output()
    
    ! Test 3: Notebook with verbose mode
    call test_notebook_verbose()
    
    if (all_tests_passed) then
        print *, "All notebook system tests passed!"
        stop 0
    else
        print *, "Some notebook system tests failed!"
        stop 1
    end if
    
contains

    subroutine test_simple_notebook_system()
        print *, "Test 1: Simple notebook execution..."
        
        ! Create test notebook file
        test_file = "test_notebook_simple.f"
        call create_test_notebook(test_file)
        
        ! Run fortran with notebook mode
        cmd = "fpm run fortran -- --notebook " // trim(test_file)
        call execute_command_line(cmd, exitstat=exit_code)
        
        if (exit_code /= 0) then
            print *, "  FAILED: Notebook execution returned non-zero exit code"
            all_tests_passed = .false.
            return
        end if
        
        ! Check output file exists
        output_file = "test_notebook_simple.md"
        inquire(file=output_file, exist=all_tests_passed)
        
        if (.not. all_tests_passed) then
            print *, "  FAILED: Output file not created"
            return
        end if
        
        ! Check output content
        call read_file_content(output_file, output_content)
        
        if (index(output_content, "# Test Notebook") == 0) then
            print *, "  FAILED: Markdown title not found in output"
            all_tests_passed = .false.
        else if (index(output_content, "```fortran") == 0) then
            print *, "  FAILED: Code block not found in output"
            all_tests_passed = .false.
        else
            print *, "  PASSED"
        end if
        
        ! Cleanup
        call delete_file(test_file)
        call delete_file(output_file)
        
    end subroutine test_simple_notebook_system
    
    subroutine test_notebook_custom_output()
        print *, "Test 2: Notebook with custom output..."
        
        test_file = "test_notebook_custom.f"
        call create_test_notebook(test_file)
        
        ! Run with custom output
        output_file = "my_custom_output.md"
        cmd = "fpm run fortran -- --notebook -o " // trim(output_file) // " " // trim(test_file)
        call execute_command_line(cmd, exitstat=exit_code)
        
        if (exit_code /= 0) then
            print *, "  FAILED: Execution failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Check custom output file exists
        inquire(file=output_file, exist=all_tests_passed)
        
        if (all_tests_passed) then
            print *, "  PASSED"
        else
            print *, "  FAILED: Custom output file not created"
        end if
        
        ! Cleanup
        call delete_file(test_file)
        call delete_file(output_file)
        
    end subroutine test_notebook_custom_output
    
    subroutine test_notebook_verbose()
        print *, "Test 3: Notebook with verbose mode..."
        
        test_file = "test_notebook_verbose.f"
        call create_test_notebook(test_file)
        
        ! Run with verbose mode, capture output
        cmd = "fpm run fortran -- --notebook -v " // trim(test_file) // " 2>&1"
        
        ! For now, just check it runs without error
        call execute_command_line(cmd, exitstat=exit_code)
        
        if (exit_code == 0) then
            print *, "  PASSED"
        else
            print *, "  FAILED: Verbose mode execution failed"
            all_tests_passed = .false.
        end if
        
        ! Cleanup
        call delete_file(test_file)
        call delete_file("test_notebook_verbose.md")
        
    end subroutine test_notebook_verbose
    
    ! Helper procedures
    
    subroutine create_test_notebook(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(a)') '! %% [markdown]'
        write(unit, '(a)') '! # Test Notebook'
        write(unit, '(a)') '! This is a test notebook.'
        write(unit, '(a)') '! %%'
        write(unit, '(a)') 'x = 42'
        write(unit, '(a)') 'print *, "The answer is", x'
        close(unit)
        
    end subroutine create_test_notebook
    
    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, iostat
        
        open(newunit=unit, file=filename, status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
    end subroutine delete_file
    
    subroutine read_file_content(filename, content)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, iostat
        
        open(newunit=unit, file=filename, status='old', &
             access='stream', form='unformatted', iostat=iostat)
        
        if (iostat /= 0) then
            content = ""
            return
        end if
        
        inquire(unit=unit, size=file_size)
        allocate(character(len=file_size) :: content)
        
        read(unit, iostat=iostat) content
        close(unit)
        
    end subroutine read_file_content
    
end program test_notebook_system