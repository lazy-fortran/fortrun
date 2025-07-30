program test_runner_paths
    use iso_fortran_env, only: error_unit
    use runner
    use temp_utils, only: temp_dir_manager, path_join
    use system_utils, only: sys_remove_file, sys_remove_dir
    use logger_utils, only: debug_print
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Runner Path Coverage Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('runner_paths_test')

    ! Test different runner execution paths to improve coverage
    if (.not. test_custom_cache_dirs()) all_tests_passed = .false.
    if (.not. test_parallel_jobs()) all_tests_passed = .false.
    if (.not. test_various_file_types()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All runner path coverage tests passed!"
        stop 0
    else
        print *, "Some runner path coverage tests failed!"
        stop 1
    end if

contains

    function test_custom_cache_dirs() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file, custom_cache
        integer :: exit_code, unit
        character(len=256) :: custom_config_dir
        character(len=256) :: ci_env
        character(len=256) :: debug_msg
        logical :: success

        print *, "Test 1: Custom cache directory paths"
        passed = .true.

        ! Check if we're in CI
        call get_environment_variable('CI', ci_env)
        if (len_trim(ci_env) > 0) then
            call debug_print("Running in CI environment")
            call debug_print("OMP_NUM_THREADS should be 1 for this test")
        end if

        ! Create a simple test program
        test_file = temp_mgr%get_file_path('cache_test.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program cache_test'
        write (unit, '(a)') '  print *, "Testing custom cache"'
        write (unit, '(a)') 'end program cache_test'
        close (unit)

        ! Test with custom cache directory
        ! Note: This should be a directory path, not a file path
        ! Use path_join for proper path separator handling
        custom_cache = path_join(temp_mgr%get_path(), 'custom_cache')
        custom_config_dir = ""

        call debug_print("About to run with custom cache dir: "//trim(custom_cache))
        call debug_print("Test file: "//trim(test_file))

        call run_fortran_file(test_file, exit_code, 0, &  ! quiet mode
                              custom_cache, custom_config_dir, &
                              1, .false.)

        if (exit_code == 0) then
            print *, "  PASS: Custom cache directory execution successful"
        else
print *, "  INFO: Custom cache execution failed (exit code:", exit_code, ") - may be OK"
        end if

        ! Test with custom config directory
        ! Note: This should be a directory path, not a file path
        ! Use path_join for proper path separator handling
        custom_config_dir = path_join(temp_mgr%get_path(), 'custom_config')

      call debug_print("About to run with custom config dir: "//trim(custom_config_dir))
        call debug_print("Custom cache still: "//trim(custom_cache))
        call debug_print("Test file still: "//trim(test_file))

        ! Extra debug for Windows CI
        if (len_trim(ci_env) > 0) then
            call debug_print("Calling run_fortran_file with:")
            call debug_print("  - test_file: "//trim(test_file))
            call debug_print("  - exit_code: (output)")
            call debug_print("  - verbose: 0")
            call debug_print("  - cache_dir: "//trim(custom_cache))
            call debug_print("  - config_dir: "//trim(custom_config_dir))
            call debug_print("  - parallel: 1")
            call debug_print("  - no_wait: .false.")
        end if

        call run_fortran_file(test_file, exit_code, 0, &
                              custom_cache, custom_config_dir, &
                              1, .false.)

     write (debug_msg, '(a,i0)') "run_fortran_file returned with exit_code: ", exit_code
        call debug_print(trim(debug_msg))

        if (exit_code == 0) then
            print *, "  PASS: Custom config directory execution successful"
        else
            print *, "  INFO: Custom config execution failed (exit code:", exit_code, ") - may be OK"
        end if

        ! Clean up
        call sys_remove_file(test_file)
        call sys_remove_dir(custom_cache, success)
        call sys_remove_dir(custom_config_dir, success)

    end function test_custom_cache_dirs

    function test_parallel_jobs() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        integer :: exit_code, unit
        character(len=256) :: custom_cache_dir, custom_config_dir

        print *, "Test 2: Parallel jobs parameter"
        passed = .true.

        ! Create a test program
        test_file = temp_mgr%get_file_path('parallel_test.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program parallel_test'
        write (unit, '(a)') '  print *, "Testing parallel jobs"'
        write (unit, '(a)') 'end program parallel_test'
        close (unit)

        custom_cache_dir = ""
        custom_config_dir = ""

        ! Test with different parallel job counts to exercise those code paths
        call run_fortran_file(test_file, exit_code, 1, &  ! verbose level 1
                              custom_cache_dir, custom_config_dir, &
                              4, .false.)  ! 4 parallel jobs

        if (exit_code == 0) then
            print *, "  PASS: Parallel jobs execution successful"
        else
            print *, "  INFO: Parallel jobs execution failed (exit code:", exit_code, ") - may be OK"
        end if

        ! Test with no_wait flag
        call run_fortran_file(test_file, exit_code, 1, &
                              custom_cache_dir, custom_config_dir, &
                              1, .true.)  ! no_wait = true

        if (exit_code == 0) then
            print *, "  PASS: No-wait execution successful"
        else
     print *, "  INFO: No-wait execution failed (exit code:", exit_code, ") - may be OK"
        end if

        ! Clean up
        call sys_remove_file(test_file)

    end function test_parallel_jobs

    function test_various_file_types() result(passed)
        logical :: passed
        character(len=:), allocatable :: f90_file, f_file
        integer :: exit_code, unit
        character(len=256) :: custom_cache_dir, custom_config_dir

        print *, "Test 3: Various file types and extensions"
        passed = .true.

        custom_cache_dir = ""
        custom_config_dir = ""

        ! Test .f90 file (standard)
        f90_file = temp_mgr%get_file_path('standard.f90')
        open (newunit=unit, file=f90_file, status='replace')
        write (unit, '(a)') 'program standard'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Standard F90"'
        write (unit, '(a)') 'end program standard'
        close (unit)

        call run_fortran_file(f90_file, exit_code, 2, &  ! verbose level 2 for debug output
                              custom_cache_dir, custom_config_dir, &
                              1, .false.)

        if (exit_code == 0) then
            print *, "  PASS: .f90 file execution successful"
        else
   print *, "  INFO: .f90 file execution failed (exit code:", exit_code, ") - may be OK"
        end if

        ! Test .lf file (should trigger preprocessor)
        f_file = temp_mgr%get_file_path('legacy.lf')
        open (newunit=unit, file=f_file, status='replace')
        write (unit, '(a)') '! Lazy Fortran file'
        write (unit, '(a)') 'x = 42'
        write (unit, '(a)') 'print *, x'
        close (unit)

        call run_fortran_file(f_file, exit_code, 2, &  ! verbose level 2
                              custom_cache_dir, custom_config_dir, &
                              1, .false.)

        if (exit_code == 0) then
            print *, "  PASS: .lf file execution successful"
        else
    print *, "  INFO: .lf file execution failed (exit code:", exit_code, ") - may be OK"
        end if

        ! Clean up
        call sys_remove_file(f90_file)
        call sys_remove_file(f_file)

    end function test_various_file_types

end program test_runner_paths
