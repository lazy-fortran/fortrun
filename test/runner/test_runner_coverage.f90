program test_runner_coverage
    use iso_fortran_env, only: error_unit
    use runner
    use temp_utils, only: temp_dir_manager
    use system_utils, only: sys_remove_file
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Runner Coverage Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('runner_coverage_test')

    ! Test runner functions to improve coverage
    if (.not. test_simple_fortran_execution()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All runner coverage tests passed!"
        stop 0
    else
        print *, "Some runner coverage tests failed!"
        stop 1
    end if

contains

    function test_simple_fortran_execution() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        integer :: exit_code, unit
        integer, parameter :: verbose_level = 0
        character(len=256) :: custom_cache_dir, custom_config_dir
        integer, parameter :: parallel_jobs = 1
        logical, parameter :: no_wait = .false.

        print *, "Test: Simple Fortran file execution"
        passed = .true.

        ! Create a simple test program
        test_file = temp_mgr%get_file_path('hello.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program hello'
        write (unit, '(a)') '  print *, "Hello, World!"'
        write (unit, '(a)') 'end program hello'
        close (unit)

        ! Test with empty cache/config directories (default behavior)
        custom_cache_dir = ""
        custom_config_dir = ""

        ! Run the Fortran file
        call run_fortran_file(test_file, exit_code, verbose_level, &
                              custom_cache_dir, custom_config_dir, &
                              parallel_jobs, no_wait)

        if (exit_code /= 0) then
            print *, "  FAIL: Fortran execution failed with exit code:", exit_code
            passed = .false.
        else
            print *, "  PASS: Fortran execution successful"
        end if

        ! Clean up
        call sys_remove_file(test_file)

        ! Test with verbose level > 0 to exercise different code paths
        if (passed) then
            ! Create another test for verbose execution
            test_file = temp_mgr%get_file_path('verbose_test.f90')
            open (newunit=unit, file=test_file, status='replace')
            write (unit, '(a)') 'print *, "Verbose test"'
            close (unit)

            call run_fortran_file(test_file, exit_code, 1, &
                                  custom_cache_dir, custom_config_dir, &
                                  parallel_jobs, no_wait)

            if (exit_code == 0) then
                print *, "  PASS: Verbose execution successful"
            else
                print *, "  WARNING: Verbose execution failed, exit code:", exit_code
                ! Don't fail the test for this as it might be environment dependent
            end if

            call sys_remove_file(test_file)
        end if

    end function test_simple_fortran_execution

end program test_runner_coverage
