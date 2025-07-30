program test_build_failure_recovery
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    use system_utils, only: sys_copy_file
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: test_file, bad_file, good_file
    character(len=1024) :: command
    integer :: test_count, pass_count, exit_code, unit, ios
    logical :: success

    test_count = 0
    pass_count = 0

    print *, "=== Build Failure Recovery Tests ==="
    print *, ""

    call temp_mgr%create('build_failure_test')

    ! Test 1: Syntax error recovery
    call test_syntax_error_recovery()

    ! Test 2: Missing module recovery
    call test_missing_module_recovery()

    ! Test 3: Circular dependency detection
    call test_circular_dependency_detection()

    ! Test 4: Malformed file recovery
    call test_malformed_file_recovery()

    ! Test 5: Build after fix
    call test_build_after_fix()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All build failure recovery tests passed!"
        stop 0
    else
        print *, "Some build failure recovery tests failed!"
        stop 1
    end if

contains

    subroutine test_syntax_error_recovery()
        call test_start("Syntax error recovery")

        ! Create a file with syntax errors
        bad_file = temp_mgr%get_file_path('syntax_error.f90')
        open (newunit=unit, file=bad_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program syntax_error'
        write (unit, '(A)') '    integer :: x'
        write (unit, '(A)') '    x = 1 ++'  ! Syntax error
        write (unit, '(A)') '    print *, x'
        write (unit, '(A)') 'end program'  ! Missing proper structure
        close (unit)

        ! Try to run - should fail gracefully
        command = fortran_with_isolated_cache('syntax_error_test')//' "'// &
                  trim(bad_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Should fail with non-zero exit code, but not crash
        success = (exit_code /= 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Syntax error recovery failed - expected failure but got success"
        end if
    end subroutine test_syntax_error_recovery

    subroutine test_missing_module_recovery()
        call test_start("Missing module recovery")

        ! Create a file that uses a non-existent module
        test_file = temp_mgr%get_file_path('missing_module.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program missing_module'
        write (unit, '(A)') '    use nonexistent_module, only: some_function'
        write (unit, '(A)') '    print *, some_function()'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Try to run - should fail gracefully with clear error
        command = fortran_with_isolated_cache('missing_module_test')//' "'// &
                  trim(test_file)//'" 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Should fail but handle the error gracefully
        success = (exit_code /= 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Missing module recovery failed"
        end if
    end subroutine test_missing_module_recovery

    subroutine test_circular_dependency_detection()
        character(len=256) :: module_a, module_b, main_circular

        call test_start("Circular dependency detection")

        ! Create module A that depends on module B
        module_a = temp_mgr%get_file_path('circular_a.f90')
        open (newunit=unit, file=module_a, status='replace', iostat=ios)
        write (unit, '(A)') 'module circular_a'
        write (unit, '(A)') '    use circular_b, only: b_function'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'contains'
        write (unit, '(A)') '    function a_function() result(val)'
        write (unit, '(A)') '        integer :: val'
        write (unit, '(A)') '        val = b_function() + 1'
        write (unit, '(A)') '    end function'
        write (unit, '(A)') 'end module circular_a'
        close (unit)

        ! Create module B that depends on module A (circular)
        module_b = temp_mgr%get_file_path('circular_b.f90')
        open (newunit=unit, file=module_b, status='replace', iostat=ios)
        write (unit, '(A)') 'module circular_b'
        write (unit, '(A)') '    use circular_a, only: a_function'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'contains'
        write (unit, '(A)') '    function b_function() result(val)'
        write (unit, '(A)') '        integer :: val'
        write (unit, '(A)') '        val = a_function() + 1'
        write (unit, '(A)') '    end function'
        write (unit, '(A)') 'end module circular_b'
        close (unit)

        ! Create main that uses one of the modules
        main_circular = temp_mgr%get_file_path('main_circular.f90')
        open (newunit=unit, file=main_circular, status='replace', iostat=ios)
        write (unit, '(A)') 'program main_circular'
        write (unit, '(A)') '    use circular_a, only: a_function'
        write (unit, '(A)') '    print *, a_function()'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Try to build - should detect circular dependency
        command = fortran_with_isolated_cache('circular_test')//' "'// &
                  trim(main_circular)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Should fail due to circular dependency
        success = (exit_code /= 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Circular dependency detection failed"
        end if
    end subroutine test_circular_dependency_detection

    subroutine test_malformed_file_recovery()
        call test_start("Malformed file recovery")

        ! Create a completely malformed file
        test_file = temp_mgr%get_file_path('malformed.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') '!@#$%^&*()_+|}{":?><'
        write (unit, '(A)') 'random garbage not fortran code'
        write (unit, '(A)') '12345 ### $$$ %%% @@@'
        close (unit)

        ! Try to run - should handle gracefully
        command = fortran_with_isolated_cache('malformed_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Should fail but not crash
        success = (exit_code /= 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Malformed file recovery failed"
        end if
    end subroutine test_malformed_file_recovery

    subroutine test_build_after_fix()
        call test_start("Build after fixing errors")

        ! Create a broken file
        test_file = temp_mgr%get_file_path('fix_test.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program fix_test'
        write (unit, '(A)') '    integer :: x'
        write (unit, '(A)') '    x = 1 ++'  ! Syntax error
        write (unit, '(A)') '    print *, x'
        write (unit, '(A)') 'end program'
        close (unit)

        ! First run should fail
        command = fortran_with_isolated_cache('fix_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        if (exit_code == 0) then
            success = .false.
            call test_result(success)
            print *, "  Expected first run to fail, but it succeeded"
            return
        end if

        ! Fix the file
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program fix_test'
        write (unit, '(A)') '    integer :: x'
        write (unit, '(A)') '    x = 1 + 1'  ! Fixed syntax
        write (unit, '(A)') '    print *, x'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Second run should succeed
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Build after fix failed - fixed code should compile"
        end if
    end subroutine test_build_after_fix

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

end program test_build_failure_recovery
