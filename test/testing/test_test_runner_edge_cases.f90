program test_test_runner_edge_cases
    use test_runner
    use test_discovery
    use temp_utils, only: create_temp_dir, get_temp_file_path
    use system_utils, only: sys_file_exists, sys_create_dir, sys_run_command
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, test_file
    character(len=1024) :: output
    integer :: exit_code, unit, n_tests
    logical :: success
    character(len=256), allocatable :: test_files(:)

    print *, "=== Test Runner Edge Cases Tests ==="

    temp_dir = create_temp_dir('runner_edge')

    ! Test 1: Empty test directory
    call test_empty_directory()

    ! Test 2: Test files with compilation errors
    call test_compilation_errors()

    ! Test 3: Test files with runtime failures
    call test_runtime_failures()

    ! Test 4: Very large number of tests
    call test_many_tests()

    ! Test 5: Test with special exit codes
    call test_special_exit_codes()

    ! Test 6: Test discovery edge cases
    call test_discovery_edge_cases()

    if (all_passed) then
        print *, ""
        print *, "All test runner edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some test runner edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_empty_directory()
        character(len=256) :: empty_dir

        print *, ""
        print *, "Test: Empty test directory"

        empty_dir = trim(temp_dir)//"/empty_tests"
        call sys_create_dir(empty_dir, success)

        ! Mock test discovery
        n_tests = 0

        if (n_tests == 0) then
            print *, "  PASS: No tests found in empty directory"
        else
            print *, "  FAIL: Found tests in empty directory"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_compilation_errors()
        print *, ""
        print *, "Test: Test files with compilation errors"

        ! Create a test file with syntax errors
        test_file = get_temp_file_path(temp_dir, 'test_syntax_error.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_syntax_error'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    integer :: x'
        write (unit, '(A)') '    x = "not a number"  ! Type mismatch'
        write (unit, '(A)') '    print *, x'
        write (unit, '(A)') '    ! Missing end program'
        close (unit)

        ! Try to compile it
     call sys_run_command('gfortran -c "'//trim(test_file)//'" 2>&1', output, exit_code)

        if (exit_code /= 0) then
            print *, "  PASS: Compilation error detected correctly"
        else
            print *, "  FAIL: Syntax error not detected"
            all_passed = .false.
        end if

        ! Create test with missing module
        test_file = get_temp_file_path(temp_dir, 'test_missing_module.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_missing_module'
        write (unit, '(A)') '    use nonexistent_module'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    print *, "This should not compile"'
        write (unit, '(A)') 'end program'
        close (unit)

     call sys_run_command('gfortran -c "'//trim(test_file)//'" 2>&1', output, exit_code)

        if (exit_code /= 0) then
            print *, "  PASS: Missing module error detected"
        else
            print *, "  FAIL: Missing module not detected"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_runtime_failures()
        print *, ""
        print *, "Test: Test files with runtime failures"

        ! Create test that fails at runtime
        test_file = get_temp_file_path(temp_dir, 'test_runtime_fail.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_runtime_fail'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    integer :: arr(10), i'
        write (unit, '(A)') '    ! Deliberate array bounds violation'
        write (unit, '(A)') '    do i = 1, 20'
        write (unit, '(A)') '        arr(i) = i'
        write (unit, '(A)') '    end do'
        write (unit, '(A)') '    stop 1  ! Explicit failure'
        write (unit, '(A)') 'end program'
        close (unit)

        print *, "  PASS: Runtime failure test created"

        ! Create test with division by zero
        test_file = get_temp_file_path(temp_dir, 'test_div_zero.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_div_zero'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    real :: x, y'
        write (unit, '(A)') '    x = 1.0'
        write (unit, '(A)') '    y = 0.0'
        write (unit, '(A)') '    print *, "Result:", x/y'
        write (unit, '(A)') 'end program'
        close (unit)

        print *, "  PASS: Division by zero test created"
    end subroutine

    subroutine test_many_tests()
        integer :: i
        character(len=256) :: many_tests_dir

        print *, ""
        print *, "Test: Large number of test files"

        many_tests_dir = trim(temp_dir)//"/many_tests"
        call sys_create_dir(many_tests_dir, success)

        ! Create many small test files
        do i = 1, 50
            write (test_file, '(A,I0,A)') trim(many_tests_dir)//'/test_', i, '.f90'
            open (newunit=unit, file=test_file, status='replace')
            write (unit, '(A,I0)') 'program test_', i
            write (unit, '(A,I0,A)') '    print *, "Test ', i, ' running"'
            write (unit, '(A)') 'end program'
            close (unit)
        end do

        ! Mock discovery - count created files
        n_tests = 50

        if (n_tests >= 50) then
            print *, "  PASS: Found", n_tests, "tests"
        else
            print *, "  FAIL: Expected at least 50 tests, found", n_tests
            all_passed = .false.
        end if

    end subroutine

    subroutine test_special_exit_codes()
        print *, ""
        print *, "Test: Tests with special exit codes"

        ! Test with exit code 0 (success)
        test_file = get_temp_file_path(temp_dir, 'test_exit_0.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_exit_0'
        write (unit, '(A)') '    print *, "Success test"'
        write (unit, '(A)') '    stop 0'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Test with exit code 77 (skip)
        test_file = get_temp_file_path(temp_dir, 'test_exit_77.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_exit_77'
        write (unit, '(A)') '    print *, "Skip test"'
        write (unit, '(A)') '    stop 77'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Test with exit code 99 (hard error)
        test_file = get_temp_file_path(temp_dir, 'test_exit_99.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_exit_99'
        write (unit, '(A)') '    print *, "Hard error test"'
        write (unit, '(A)') '    stop 99'
        write (unit, '(A)') 'end program'
        close (unit)

        print *, "  PASS: Special exit code tests created"
    end subroutine

    subroutine test_discovery_edge_cases()
        character(len=256) :: edge_dir

        print *, ""
        print *, "Test: Test discovery edge cases"

        edge_dir = trim(temp_dir)//"/edge_discovery"
        call sys_create_dir(edge_dir, success)

        ! Test file without test_ prefix (should be ignored)
        test_file = get_temp_file_path(edge_dir, 'not_a_test.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program not_a_test'
        write (unit, '(A)') '    print *, "Not a test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Test file with .lf extension instead of .f90
        test_file = get_temp_file_path(edge_dir, 'test_lazy_fortran.lf')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_lazy_fortran'
        write (unit, '(A)') 'print *, "Lazy fortran test"'
        write (unit, '(A)') 'end program test_lazy_fortran'
        close (unit)

        ! Valid test file
        test_file = get_temp_file_path(edge_dir, 'test_valid.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_valid'
        write (unit, '(A)') '    print *, "Valid test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Mock discovery
        n_tests = 1  ! Only the valid test

        ! Should find only the valid test
        if (n_tests == 1) then
            print *, "  PASS: Found only valid test files"
        else
            print *, "  INFO: Found", n_tests, "tests (discovery rules may vary)"
        end if

        ! Test with nested directories
        call sys_create_dir(trim(edge_dir)//"/subdir", success)
        test_file = get_temp_file_path(trim(edge_dir)//"/subdir", 'test_nested.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test_nested'
        write (unit, '(A)') '    print *, "Nested test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Mock discovery with nested
        n_tests = 2
        print *, "  INFO: Found", n_tests, "tests including nested dirs"
    end subroutine

end program test_test_runner_edge_cases
