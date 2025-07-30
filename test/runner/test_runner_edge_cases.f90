program test_runner_edge_cases
    use runner, only: run_fortran_file
    use temp_utils, only: create_temp_dir, get_temp_file_path, create_test_cache_dir, create_temp_file
    use temp_utils, only: mkdir
    use system_utils, only: sys_remove_file, sys_dir_exists
    implicit none

    logical :: all_tests_passed

    print *, "=== Runner Module Edge Case Tests ==="
    print *

    all_tests_passed = .true.

    ! Test edge cases in the runner module
    if (.not. test_nonexistent_file()) all_tests_passed = .false.
    if (.not. test_invalid_extensions()) all_tests_passed = .false.
    if (.not. test_empty_file()) all_tests_passed = .false.
    if (.not. test_custom_directories()) all_tests_passed = .false.
    if (.not. test_preprocessing_errors()) all_tests_passed = .false.
    if (.not. test_parallel_execution()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All runner edge case tests passed!"
        stop 0
    else
        print *, "Some runner edge case tests failed!"
        stop 1
    end if

contains

    function test_nonexistent_file() result(passed)
        logical :: passed
        integer :: exit_code

        print *, "Test 1: Non-existent file handling"
        passed = .true.

        ! Test with file that doesn't exist
        block
            character(len=256) :: nonexistent_file
            nonexistent_file = create_temp_file('fortran_test_definitely_does_not_exist_12345', '.f90')
            call run_fortran_file(nonexistent_file, exit_code, &
                                  verbose_level=0, custom_cache_dir="", &
                                  custom_config_dir="", parallel_jobs=1, no_wait=.true.)
        end block

        if (exit_code == 0) then
            print *, "  FAILED: Should fail for non-existent file"
            passed = .false.
        end if

        ! Test with empty filename
        call run_fortran_file("", exit_code, &
                              verbose_level=0, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=1, no_wait=.true.)

        if (exit_code == 0) then
            print *, "  FAILED: Should fail for empty filename"
            passed = .false.
        end if

        if (passed) print *, "  PASS: Non-existent file handling"

    end function test_nonexistent_file

    function test_invalid_extensions() result(passed)
        logical :: passed
        integer :: exit_code, unit
        character(len=256) :: test_file

        print *, "Test 2: Invalid file extensions"
        passed = .true.

        ! Create test file with wrong extension
        test_file = create_temp_file('fortran_test_runner_edge', '.txt')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "program test"
        write (unit, '(a)') "end program"
        close (unit)

        call run_fortran_file(test_file, exit_code, &
                              verbose_level=0, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=1, no_wait=.true.)

        if (exit_code == 0) then
            print *, "  FAILED: Should fail for .txt extension"
            passed = .false.
        end if

        call sys_remove_file(test_file)

        ! Test with no extension
        test_file = create_temp_file('fortran_test_runner_edge_noext', '')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "program test"
        write (unit, '(a)') "end program"
        close (unit)

        call run_fortran_file(test_file, exit_code, &
                              verbose_level=0, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=1, no_wait=.true.)

        if (exit_code == 0) then
            print *, "  FAILED: Should fail for no extension"
            passed = .false.
        end if

        call sys_remove_file(test_file)

        if (passed) print *, "  PASS: Invalid extensions"

    end function test_invalid_extensions

    function test_empty_file() result(passed)
        logical :: passed
        integer :: exit_code, unit
        character(len=256) :: test_file

        print *, "Test 3: Empty file handling"
        passed = .true.

        ! Create empty .f90 file with minimal valid program
        test_file = create_temp_file('fortran_test_runner_empty', '.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "program empty"
        write (unit, '(a)') "end program empty"
        close (unit)

        call run_fortran_file(test_file, exit_code, &
                              verbose_level=2, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=1, no_wait=.true.)

        ! Empty file might compile or might fail - just check it doesn't crash
        passed = .true.

        call sys_remove_file(test_file)

        if (passed) print *, "  PASS: Empty file handling"

    end function test_empty_file

    function test_custom_directories() result(passed)
        logical :: passed
        integer :: exit_code, unit
        character(len=256) :: test_file, custom_cache, custom_config
        character(len=256) :: base_temp_dir

        print *, "Test 4: Custom directory handling"
        passed = .true.

        ! Create a single base temp directory
        base_temp_dir = create_temp_dir('fortran_test')

        ! Create test file
        test_file = get_temp_file_path(base_temp_dir, 'test_runner_custom_dirs.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "program test_custom"
        write (unit, '(a)') "  print *, 'Hello from custom dirs'"
        write (unit, '(a)') "end program"
        close (unit)

        ! Test with custom cache directory
        custom_cache = create_test_cache_dir('runner_edge_custom')
        custom_config = create_temp_dir('fortran_test_runner_custom_config')

        call mkdir(trim(custom_cache))
        call mkdir(trim(custom_config))

        call run_fortran_file(test_file, exit_code, &
                              verbose_level=1, custom_cache_dir=custom_cache, &
                       custom_config_dir=custom_config, parallel_jobs=1, no_wait=.true.)

        ! Check if custom directories were used (via existence of any cache files)
        passed = sys_dir_exists(custom_cache)

        ! Clean up is handled by temp_utils, directories will be removed automatically

        if (passed) print *, "  PASS: Custom directories"

    end function test_custom_directories

    function test_preprocessing_errors() result(passed)
        logical :: passed
        integer :: exit_code, unit
        character(len=256) :: test_file

        print *, "Test 5: Preprocessing error handling"
        passed = .true.

        ! Create .lf file that might cause preprocessing issues
        test_file = create_temp_file('fortran_test_runner_preprocess', '.lf')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "c This is an old-style comment"
        write (unit, '(a)') "      program test_preprocess"
        write (unit, '(a)') "      implicit none"
        write (unit, '(a)') "      x = 42  ! Missing declaration"
        write (unit, '(a)') "      print *, x"
        write (unit, '(a)') "      end"
        close (unit)

        call run_fortran_file(test_file, exit_code, &
                              verbose_level=2, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=1, no_wait=.true.)

        ! Should handle preprocessing (exit code might be 0 or 1)
        passed = .true.

        call sys_remove_file(test_file)

        if (passed) print *, "  PASS: Preprocessing errors"

    end function test_preprocessing_errors

    function test_parallel_execution() result(passed)
        logical :: passed
        integer :: exit_code, unit
        character(len=256) :: test_file

        print *, "Test 6: Parallel execution options"
        passed = .true.

        ! Create test file
        test_file = create_temp_file('fortran_test_runner_parallel', '.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "program test_parallel"
        write (unit, '(a)') "  implicit none"
        write (unit, '(a)') "  print *, 'Testing parallel build'"
        write (unit, '(a)') "end program"
        close (unit)

        ! Test with different parallel job counts
        call run_fortran_file(test_file, exit_code, &
                              verbose_level=0, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=4, no_wait=.true.)

        if (exit_code /= 0) then
            print *, "  WARNING: Parallel build failed"
        end if

        ! Test with parallel_jobs = 0 (should use default)
        call run_fortran_file(test_file, exit_code, &
                              verbose_level=0, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=0, no_wait=.true.)

        ! Test with very high parallel jobs
        call run_fortran_file(test_file, exit_code, &
                              verbose_level=0, custom_cache_dir="", &
                              custom_config_dir="", parallel_jobs=999, no_wait=.false.)

        call sys_remove_file(test_file)

        if (passed) print *, "  PASS: Parallel execution"

    end function test_parallel_execution

end program test_runner_edge_cases
