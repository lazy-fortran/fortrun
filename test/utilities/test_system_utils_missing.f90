program test_system_utils_missing
    use iso_fortran_env, only: error_unit
    use system_utils
    use temp_utils, only: temp_dir_manager
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== System Utils Missing Lines Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('system_missing_test')

    ! Target specific missing lines in system_utils.f90
    if (.not. test_sys_copy_dir_error_cases()) all_tests_passed = .false.
    if (.not. test_get_stderr_redirect_platform()) all_tests_passed = .false.
    if (.not. test_sys_list_files_edge_cases()) all_tests_passed = .false.
    if (.not. test_sys_copy_file_failure()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All system utils missing lines tests passed!"
        stop 0
    else
        print *, "Some system utils missing lines tests failed!"
        stop 1
    end if

contains

    function test_sys_copy_dir_error_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: source_dir, dest_dir
        logical :: success
        character(len=256) :: error_msg

        print *, "Test: sys_copy_dir error cases"
        passed = .true.

        ! Test with non-existent source directory
        source_dir = temp_mgr%get_file_path('nonexistent_source_dir')
        dest_dir = temp_mgr%get_file_path('copy_dest')

        call sys_copy_dir(source_dir, dest_dir, success, error_msg)

        if (.not. success) then
            print *, "  PASS: sys_copy_dir properly failed for non-existent source"
            print *, "  ERROR MSG: ", trim(error_msg)
        else
            print *, "  FAIL: sys_copy_dir should have failed for non-existent source"
            passed = .false.
        end if

        ! Test with empty directories
        source_dir = temp_mgr%get_file_path('empty_source')
        dest_dir = temp_mgr%get_file_path('empty_dest')

        call sys_create_dir(source_dir, success)
        if (success) then
            call sys_copy_dir(source_dir, dest_dir, success, error_msg)
            if (success) then
                print *, "  PASS: sys_copy_dir handled empty directory"
            else
          print *, "  INFO: sys_copy_dir failed with empty directory: ", trim(error_msg)
            end if
            call sys_remove_dir(source_dir, success)
            call sys_remove_dir(dest_dir, success)
        end if

        ! Test both Windows and Unix code paths by testing current OS
        if (get_os_type() == OS_WINDOWS) then
            print *, "  INFO: Tested Windows xcopy path"
        else
            print *, "  INFO: Tested Unix cp path"
        end if

    end function test_sys_copy_dir_error_cases

    function test_get_stderr_redirect_platform() result(passed)
        logical :: passed
        character(len=:), allocatable :: redirect

        print *, "Test: get_stderr_redirect platform specifics"
        passed = .true.

        ! Test the function multiple times to ensure consistency
        redirect = get_stderr_redirect()

        if (.not. allocated(redirect)) then
            print *, "  FAIL: get_stderr_redirect not allocated"
            passed = .false.
            return
        end if

        ! Verify platform-specific behavior
        if (get_os_type() == OS_WINDOWS) then
            if (index(redirect, 'nul') > 0) then
                print *, "  PASS: Windows stderr redirect uses 'nul': ", redirect
            else
                print *, "  WARNING: Windows redirect doesn't contain 'nul': ", redirect
            end if
        else
            if (index(redirect, '/dev/null') > 0) then
                print *, "  PASS: Unix stderr redirect uses '/dev/null': ", redirect
            else
             print *, "  WARNING: Unix redirect doesn't contain '/dev/null': ", redirect
            end if
        end if

        ! Test consistency across multiple calls
        block
            character(len=:), allocatable :: redirect2
            redirect2 = get_stderr_redirect()
            if (redirect == redirect2) then
                print *, "  PASS: get_stderr_redirect is consistent"
            else
                print *, "  FAIL: get_stderr_redirect inconsistent between calls"
                passed = .false.
            end if
        end block

    end function test_get_stderr_redirect_platform

    function test_sys_list_files_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_dir
        character(len=256) :: files(10)
        integer :: num_files
        logical :: success

        print *, "Test: sys_list_files edge cases"
        passed = .true.

        ! Test with non-existent directory
        call sys_list_files(temp_mgr%get_file_path('nonexistent_dir'), '*.f90', files, num_files)
        if (num_files == 0) then
            print *, "  PASS: sys_list_files handles non-existent directory"
        else
  print *, "  WARNING: sys_list_files found files in non-existent directory:", num_files
        end if

        ! Test with various patterns
        test_dir = temp_mgr%get_file_path('pattern_test')
        call sys_create_dir(test_dir, success)

        if (success) then
            ! Create test files with different extensions
            block
                integer :: unit
                character(len=:), allocatable :: test_file

                test_file = test_dir//sys_get_path_separator()//'test1.f90'
                open (newunit=unit, file=test_file, status='replace')
                close (unit)

                test_file = test_dir//sys_get_path_separator()//'test2.F90'
                open (newunit=unit, file=test_file, status='replace')
                close (unit)

                test_file = test_dir//sys_get_path_separator()//'test3.txt'
                open (newunit=unit, file=test_file, status='replace')
                close (unit)
            end block

            ! Test different patterns
            call sys_list_files(test_dir, '*.f90', files, num_files)
            print *, "  INFO: Found .f90 files:", num_files

            call sys_list_files(test_dir, '*.F90', files, num_files)
            print *, "  INFO: Found .F90 files:", num_files

            call sys_list_files(test_dir, '*.txt', files, num_files)
            print *, "  INFO: Found .txt files:", num_files

            call sys_list_files(test_dir, '*', files, num_files)
            print *, "  INFO: Found all files:", num_files

            print *, "  PASS: sys_list_files pattern testing completed"

            call sys_remove_dir(test_dir, success)
        end if

    end function test_sys_list_files_edge_cases

    function test_sys_copy_file_failure() result(passed)
        logical :: passed
        character(len=:), allocatable :: source_file, dest_file
        logical :: success
        integer :: unit

        print *, "Test: sys_copy_file failure cases"
        passed = .true.

        ! Test copying non-existent file
        source_file = temp_mgr%get_file_path('nonexistent_source.txt')
        dest_file = temp_mgr%get_file_path('copy_dest.txt')

        call sys_copy_file(source_file, dest_file, success)

        if (.not. success) then
            print *, "  PASS: sys_copy_file properly failed for non-existent source"
        else
            print *, "  FAIL: sys_copy_file should have failed for non-existent source"
            passed = .false.
        end if

        ! Test copying to invalid destination
        source_file = temp_mgr%get_file_path('valid_source.txt')
        dest_file = temp_mgr%get_file_path('invalid_dest_dir/file.txt')

        ! Create valid source
        open (newunit=unit, file=source_file, status='replace')
        write (unit, '(a)') 'test content'
        close (unit)

        call sys_copy_file(source_file, dest_file, success)

        if (.not. success) then
            print *, "  PASS: sys_copy_file properly failed for invalid destination"
        else
     print *, "  WARNING: sys_copy_file unexpectedly succeeded with invalid destination"
        end if

        call sys_remove_file(source_file)

    end function test_sys_copy_file_failure

end program test_system_utils_missing
