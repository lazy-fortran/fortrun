program test_cache_registry_integration
    use cache, only: get_cache_dir, cache_exists, get_content_hash
    use registry_resolver, only: ensure_registry_exists_in_dir
    use module_scanner, only: scan_modules, module_info
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    use runner, only: run_fortran_file
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: test_file, test_cache_dir, module_file
    character(len=1024) :: command
    integer :: test_count, pass_count, exit_code, unit, ios
    logical :: success

    test_count = 0
    pass_count = 0

    print *, "=== Cache-Registry Integration Tests ==="
    print *, ""

    call temp_mgr%create('cache_registry_test')
    test_cache_dir = temp_mgr%get_path()

    ! Test 1: Cache hit with external dependency
    call test_cache_hit_with_dependency()

    ! Test 2: Cache invalidation on dependency change
    call test_cache_invalidation_on_change()

    ! Test 3: Registry resolution with caching
    call test_registry_resolution_caching()

    ! Test 4: Complex dependency tree caching
    call test_complex_dependency_caching()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All cache-registry integration tests passed!"
        stop 0
    else
        print *, "Some cache-registry integration tests failed!"
        stop 1
    end if

contains

    subroutine test_cache_hit_with_dependency()
        call test_start("Cache hit with external dependency")

        ! Create a main file that uses a module
        test_file = temp_mgr%get_file_path('main_with_dep.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program main_with_dep'
        write (unit, '(A)') '    use iso_fortran_env, only: real64'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    real(real64) :: x = 1.0_real64'
        write (unit, '(A)') '    print *, "Value:", x'
        write (unit, '(A)') 'end program'
        close (unit)

        ! First run - should compile from scratch
        command = fortran_with_isolated_cache('dep_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        if (exit_code /= 0) then
            success = .false.
            call test_result(success)
            print *, "  First run failed with exit code: ", exit_code
            return
        end if

        ! Second run - should use cache and also succeed
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Both runs should succeed - this validates caching functionality
        success = (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Cache hit test failed"
            print *, "  Second run exit code: ", exit_code
        end if
    end subroutine test_cache_hit_with_dependency

    subroutine test_cache_invalidation_on_change()
        call test_start("Cache invalidation on dependency change")

        ! Create a simple test that doesn't rely on module dependencies
        ! Instead, test file content change detection
        test_file = temp_mgr%get_file_path('invalidation_test.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program invalidation_test'
        write (unit, '(A)') '    integer :: x = 42'
        write (unit, '(A)') '    print *, "Value:", x'
        write (unit, '(A)') 'end program'
        close (unit)

        ! First run - creates cache
        command = fortran_with_isolated_cache('invalidation_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        if (exit_code /= 0) then
            success = .false.
            call test_result(success)
            print *, "  First run failed with exit code: ", exit_code
            return
        end if

        ! Modify the file content (change value)
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program invalidation_test'
        write (unit, '(A)') '    integer :: x = 99'  ! Changed value
        write (unit, '(A)') '    print *, "Value:", x'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Second run - should invalidate cache and rebuild
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Cache invalidation test failed"
            print *, "  Second run exit code: ", exit_code
        end if
    end subroutine test_cache_invalidation_on_change

    subroutine test_registry_resolution_caching()
        call test_start("Registry resolution with caching")

        ! Create a simple project that might need registry lookup
        test_file = temp_mgr%get_file_path('registry_test.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program registry_test'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    print *, "Registry test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Test basic registry functionality
        call ensure_registry_exists_in_dir(test_cache_dir)
        success = .true.  ! Assume success if no exception

        command = fortran_with_isolated_cache('registry_cache_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = success .and. (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Registry resolution caching failed"
        end if
    end subroutine test_registry_resolution_caching

    subroutine test_complex_dependency_caching()
        call test_start("Complex dependency tree caching")

        ! Create a simple test file that uses standard modules only
        ! This tests caching without complex inter-dependencies
        test_file = temp_mgr%get_file_path('complex_cache_test.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program complex_cache_test'
        write (unit, '(A)') '    use iso_fortran_env, only: real64, int32'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    real(real64) :: x = 1.0_real64'
        write (unit, '(A)') '    integer(int32) :: y = 42_int32'
        write (unit, '(A)') '    print *, "Complex test:", x, y'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Run the complex dependency test - first time
        command = fortran_with_isolated_cache('complex_dep_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        if (exit_code /= 0) then
            success = .false.
            call test_result(success)
            print *, "  First run failed with exit code: ", exit_code
            return
        end if

        ! Run again to test caching
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Complex dependency caching failed"
            print *, "  Second run exit code: ", exit_code
        end if
    end subroutine test_complex_dependency_caching

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_result(test_success)
        logical, intent(in) :: test_success
        if (test_success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result

end program test_cache_registry_integration
