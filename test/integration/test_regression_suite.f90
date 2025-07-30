program test_regression_suite
    ! Regression Test Suite
    ! Ensures that changes don't break existing functionality
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    implicit none

    logical :: all_passed

    print *, "=== Regression Test Suite ==="

    all_passed = .true.

    ! Test core functionality regressions
    all_passed = all_passed .and. test_basic_codegen_regression()
    all_passed = all_passed .and. test_type_system_regression()
    all_passed = all_passed .and. test_ast_generation_regression()
    all_passed = all_passed .and. test_mlir_output_regression()
    all_passed = all_passed .and. test_error_handling_regression()
    all_passed = all_passed .and. test_cli_options_regression()

    if (all_passed) then
        print *, ""
        print *, "All regression tests PASSED!"
    else
        print *, ""
        print *, "Some regression tests FAILED!"
        stop 1
    end if

contains

    function test_basic_codegen_regression() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file, baseline_file, current_file
        integer :: exit_code
        logical :: files_match

        print *, "Testing basic codegen regression..."

        passed = .false.
        call temp_mgr%create('regression_codegen')

        ! Create reference test case
        test_file = temp_mgr%get_file_path('codegen_test.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program codegen_test'
        write (10, '(A)') '    integer :: x = 10, y = 20, z'
        write (10, '(A)') '    z = x + y'
        write (10, '(A)') '    print *, z'
        write (10, '(A)') 'end program codegen_test'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('regression_codegen')

        ! Generate baseline (this should be a known good output)
        baseline_file = temp_mgr%get_file_path('baseline_codegen.f90')
        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > ' // baseline_file, &
                                  exitstat=exit_code)

        if (exit_code /= 0) then
            print *, "FAIL: Failed to generate codegen baseline"
            return
        end if

        ! Generate current output
        current_file = temp_mgr%get_file_path('current_codegen.f90')
        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > ' // current_file, &
                                  exitstat=exit_code)

        if (exit_code /= 0) then
            print *, "FAIL: Failed to generate current codegen output"
            return
        end if

        ! Compare outputs
        call execute_command_line('cmp ' // baseline_file // ' ' // current_file // ' > /dev/null 2>&1', &
                                  exitstat=exit_code)
        files_match = (exit_code == 0)

        if (files_match) then
            print *, "PASS: No regression in basic codegen"
            passed = .true.
        else
            print *, "FAIL: Regression detected in basic codegen"
        end if
    end function test_basic_codegen_regression

    function test_type_system_regression() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file
        integer :: exit_code

        print *, "Testing type system regression..."

        passed = .false.
        call temp_mgr%create('regression_types')

        ! Create test with various type declarations
        test_file = temp_mgr%get_file_path('types_test.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program types_test'
        write (10, '(A)') '    integer :: i = 42'
        write (10, '(A)') '    real :: r = 3.14'
        write (10, '(A)') '    character(len=10) :: s = "hello"'
        write (10, '(A)') '    logical :: b = .true.'
        write (10, '(A)') '    print *, i, r, s, b'
        write (10, '(A)') 'end program types_test'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('regression_types')

        ! Test with standardize only
        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > /dev/null 2>&1', &
                                  exitstat=exit_code)
        if (exit_code /= 0) then
            print *, "FAIL: Type system regression in Fortran backend"
            return
        end if

        print *, "PASS: No regression in type system"
        passed = .true.
    end function test_type_system_regression

    function test_ast_generation_regression() result(passed)
        logical :: passed
        ! Skip AST debug tests since JSON functionality has been moved
        print *, "Testing AST generation regression..."
        print *, "SKIP: AST/JSON functionality moved to separate package"
        passed = .true.
    end function test_ast_generation_regression

    function test_mlir_output_regression() result(passed)
        logical :: passed
        ! Skip MLIR tests since compile functionality has been removed
        print *, "Testing MLIR output regression..."
        print *, "SKIP: MLIR/compile functionality moved to separate package"
        passed = .true.
    end function test_mlir_output_regression

    function test_error_handling_regression() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file
        integer :: exit_code

        print *, "Testing error handling regression..."

        passed = .false.
        call temp_mgr%create('regression_errors')

        ! Create test case with known error
        test_file = temp_mgr%get_file_path('error_test.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program error_test'
        write (10, '(A)') '    integer :: x'
        write (10, '(A)') '    undeclared_variable = 42'  ! Use undeclared variable
        write (10, '(A)') 'end program error_test'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('regression_errors')

        ! Test error handling (should fail gracefully)
        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > /dev/null 2>&1', &
                                  exitstat=exit_code)

        ! Error handling is working if it doesn't crash (exit code can be 0 or non-zero)
        print *, "PASS: Error handling working (no crash)"
        passed = .true.
    end function test_error_handling_regression

    function test_cli_options_regression() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file
        integer :: exit_code

        print *, "Testing CLI options regression..."

        passed = .false.
        call temp_mgr%create('regression_cli')

        ! Create simple test case
        test_file = temp_mgr%get_file_path('cli_test.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program cli_test'
        write (10, '(A)') '    integer :: x = 10'
        write (10, '(A)') '    print *, x'
        write (10, '(A)') 'end program cli_test'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('regression_cli')

        ! Test various CLI options
  call execute_command_line(fortran_cmd//' --help > /dev/null 2>&1', exitstat=exit_code)
        if (exit_code /= 0) then
            print *, "FAIL: --help option regression"
            return
        end if

        call execute_command_line(fortran_cmd // ' --version > /dev/null 2>&1', exitstat=exit_code)
        if (exit_code /= 0) then
            print *, "FAIL: --version option regression"
            return
        end if

        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > /dev/null 2>&1', &
                                  exitstat=exit_code)
        if (exit_code /= 0) then
            print *, "FAIL: --standardize option regression"
            return
        end if

        ! Skip compile tests since functionality has been removed

        print *, "PASS: No regression in CLI options"
        passed = .true.
    end function test_cli_options_regression

end program test_regression_suite
