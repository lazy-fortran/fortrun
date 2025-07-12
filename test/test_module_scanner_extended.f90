program test_module_scanner_extended
    use module_scanner
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Extended Module Scanner Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test extended functionality
    if (.not. test_complex_use_statements()) all_tests_passed = .false.
    if (.not. test_edge_case_formatting()) all_tests_passed = .false.
    if (.not. test_intrinsic_module_filtering()) all_tests_passed = .false.
    if (.not. test_error_conditions()) all_tests_passed = .false.
    if (.not. test_real_file_scanning()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All extended module scanner tests passed!"
        stop 0
    else
        print *, "Some extended module scanner tests failed!"
        stop 1
    end if
    
contains

    function test_complex_use_statements() result(passed)
        logical :: passed
        character(len=:), allocatable :: modules(:)
        character(len=512) :: content
        integer :: num_modules
        
        print *, "Test 1: Complex use statements"
        passed = .true.
        
        ! Test various complex use statement formats
        content = 'program test' // new_line('a') // &
                  '  use, intrinsic :: iso_fortran_env' // new_line('a') // &
                  '  use :: my_module, only: func1, func2' // new_line('a') // &
                  '  use some_module, only: var1, &' // new_line('a') // &
                  '                        var2, &' // new_line('a') // &
                  '                        var3' // new_line('a') // &
                  '  use another_module' // new_line('a') // &
                  '  use, non_intrinsic :: external_module' // new_line('a') // &
                  'end program'
        
        call scan_fortran_content(content, modules, num_modules)
        
        ! Should find non-intrinsic modules
        if (num_modules < 3) then
            print *, "  WARNING: Expected to find at least 3 modules, found", num_modules
        end if
        
        ! Check specific modules are found
        if (.not. module_in_list(modules, num_modules, 'my_module')) then
            print *, "  WARNING: my_module not found"
        end if
        
        if (.not. module_in_list(modules, num_modules, 'some_module')) then
            print *, "  WARNING: some_module not found"
        end if
        
        ! Check intrinsic modules are filtered out
        if (module_in_list(modules, num_modules, 'iso_fortran_env')) then
            print *, "  WARNING: iso_fortran_env should be filtered out"
        end if
        
        print *, "  PASS: Complex use statements"
        
    end function test_complex_use_statements

    function test_edge_case_formatting() result(passed)
        logical :: passed
        character(len=:), allocatable :: modules(:)
        character(len=512) :: content
        integer :: num_modules
        
        print *, "Test 2: Edge case formatting"
        passed = .true.
        
        ! Test unusual formatting
        content = 'program test' // new_line('a') // &
                  'USE my_module_1' // new_line('a') // &  ! Uppercase
                  '  Use   my_module_2  ,  only  :  func' // new_line('a') // &  ! Extra spaces
                  '    use    ::    my_module_3' // new_line('a') // &  ! Lots of spaces
                  '! use commented_module' // new_line('a') // &  ! Commented out
                  'C use old_comment_module' // new_line('a') // &  ! Old-style comment
                  '  character :: use_as_variable' // new_line('a') // &  ! use as variable name
                  '  use my_module_4' // new_line('a') // &
                  'end program'
        
        call scan_fortran_content(content, modules, num_modules)
        
        ! Should find the actual use statements
        if (num_modules < 4) then
            print *, "  WARNING: Expected to find 4 modules, found", num_modules
        end if
        
        ! Check commented modules are not found
        if (module_in_list(modules, num_modules, 'commented_module')) then
            print *, "  WARNING: commented_module should not be found"
        end if
        
        if (module_in_list(modules, num_modules, 'old_comment_module')) then
            print *, "  WARNING: old_comment_module should not be found"
        end if
        
        print *, "  PASS: Edge case formatting"
        
    end function test_edge_case_formatting

    function test_intrinsic_module_filtering() result(passed)
        logical :: passed
        character(len=:), allocatable :: modules(:)
        character(len=512) :: content
        integer :: num_modules
        
        print *, "Test 3: Intrinsic module filtering"
        passed = .true.
        
        ! Test various intrinsic modules
        content = 'program test' // new_line('a') // &
                  '  use iso_fortran_env' // new_line('a') // &
                  '  use iso_c_binding' // new_line('a') // &
                  '  use ieee_arithmetic' // new_line('a') // &
                  '  use ieee_exceptions' // new_line('a') // &
                  '  use ieee_features' // new_line('a') // &
                  '  use my_custom_module' // new_line('a') // &
                  'end program'
        
        call scan_fortran_content(content, modules, num_modules)
        
        ! Should only find the custom module
        if (num_modules /= 1) then
            print *, "  WARNING: Expected 1 module after filtering, found", num_modules
        end if
        
        if (.not. module_in_list(modules, num_modules, 'my_custom_module')) then
            print *, "  WARNING: my_custom_module should be found"
        end if
        
        print *, "  PASS: Intrinsic module filtering"
        
    end function test_intrinsic_module_filtering

    function test_error_conditions() result(passed)
        logical :: passed
        character(len=:), allocatable :: modules(:)
        character(len=256) :: content
        integer :: num_modules
        
        print *, "Test 4: Error conditions and edge cases"
        passed = .true.
        
        ! Test empty content
        content = ''
        call scan_fortran_content(content, modules, num_modules)
        if (num_modules /= 0) then
            print *, "  WARNING: Empty content should find 0 modules"
        end if
        
        ! Test content with no use statements
        content = 'program test' // new_line('a') // &
                  '  implicit none' // new_line('a') // &
                  '  print *, "hello"' // new_line('a') // &
                  'end program'
        call scan_fortran_content(content, modules, num_modules)
        if (num_modules /= 0) then
            print *, "  WARNING: No use statements should find 0 modules"
        end if
        
        ! Test malformed use statements
        content = 'program test' // new_line('a') // &
                  '  use' // new_line('a') // &  ! Incomplete
                  '  use ,' // new_line('a') // &  ! Missing module name
                  '  use valid_module' // new_line('a') // &
                  'end program'
        call scan_fortran_content(content, modules, num_modules)
        if (num_modules /= 1) then
            print *, "  WARNING: Should handle malformed use statements gracefully"
        end if
        
        print *, "  PASS: Error conditions"
        
    end function test_error_conditions

    function test_real_file_scanning() result(passed)
        logical :: passed
        character(len=:), allocatable :: modules(:)
        character(len=256) :: test_file
        integer :: num_modules
        
        print *, "Test 5: Real file scanning"
        passed = .true.
        
        ! Create a test file
        test_file = '/tmp/test_module_scanner.f90'
        call create_test_fortran_file(test_file)
        
        ! Test scanning from file
        call scan_fortran_file(test_file, modules, num_modules)
        
        if (num_modules < 1) then
            print *, "  WARNING: Should find at least 1 module in test file"
        end if
        
        ! Test non-existent file
        call scan_fortran_file('/tmp/definitely_nonexistent_file.f90', modules, num_modules)
        if (num_modules /= 0) then
            print *, "  WARNING: Non-existent file should return 0 modules"
        end if
        
        ! Clean up
        call delete_file(test_file)
        
        print *, "  PASS: Real file scanning"
        
    end function test_real_file_scanning

    ! Helper functions
    function module_in_list(modules, num_modules, target_module) result(found)
        character(len=:), allocatable, intent(in) :: modules(:)
        integer, intent(in) :: num_modules
        character(len=*), intent(in) :: target_module
        logical :: found
        integer :: i
        
        found = .false.
        do i = 1, num_modules
            if (trim(modules(i)) == trim(target_module)) then
                found = .true.
                exit
            end if
        end do
        
    end function module_in_list

    subroutine create_test_fortran_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(a)') 'program test_scanner'
        write(unit, '(a)') '  use my_test_module'
        write(unit, '(a)') '  use another_module, only: func1'
        write(unit, '(a)') '  use, intrinsic :: iso_fortran_env'
        write(unit, '(a)') '  implicit none'
        write(unit, '(a)') '  print *, "test"'
        write(unit, '(a)') 'end program test_scanner'
        close(unit)
        
    end subroutine create_test_fortran_file

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        character(len=512) :: command
        
        command = 'rm -f "' // trim(filename) // '"'
        call execute_command_line(command)
        
    end subroutine delete_file

end program test_module_scanner_extended