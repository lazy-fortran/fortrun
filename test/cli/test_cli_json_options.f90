program test_cli_json_options
    use temp_utils, only: get_system_temp_dir, path_join, fortran_with_isolated_cache, create_test_cache_dir
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== CLI JSON Options Tests ==='
    print *

    ! Test JSON input functionality (skip help text parsing - test infrastructure issue)
    if (.not. test_json_functionality()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All CLI JSON option tests passed!'
        stop 0
    else
        print *, 'Some CLI JSON option tests failed!'
        stop 1
    end if

contains

    subroutine run_fortran_with_args(test_name, option, filename, exitstat)
        use fpm_environment, only: get_os_type, OS_WINDOWS
        character(len=*), intent(in) :: test_name, option, filename
        integer, intent(out) :: exitstat
        character(len=:), allocatable :: command
        
        if (get_os_type() == OS_WINDOWS) then
            ! On Windows, we need to handle quoting carefully
            command = 'cmd /c "set XDG_CACHE_HOME=' // trim(create_test_cache_dir(test_name)) // &
                      ' && fpm run fortran -- ' // trim(option) // ' ""' // trim(filename) // '"""'
        else
            command = trim(fortran_with_isolated_cache(test_name)) // ' ' // trim(option) // ' "' // &
                      trim(filename) // '"'
        end if
        
        call execute_command_line(command, wait=.true., exitstat=exitstat)
    end subroutine run_fortran_with_args

    logical function test_json_functionality()
        character(len=:), allocatable :: temp_dir
        test_json_functionality = .true.
        print *, 'Testing JSON pipeline functionality...'

        temp_dir = get_system_temp_dir()

        ! Test --from-tokens option
        print *, '  Testing --from-tokens...'
        block
            integer :: iostat

            ! Create a simple JSON tokens file with a basic statement
            block
                integer :: unit
                open(newunit=unit, file=path_join(temp_dir, 'test_tokens.json'), status='replace')
                write(unit, '(a)') '{"tokens": ['
                write(unit, '(a)') '  {"type": "identifier", "text": "x", "line": 1, "column": 1},'
                write(unit, '(a)') '  {"type": "operator", "text": "=", "line": 1, "column": 3},'
                write(unit, '(a)') '  {"type": "number", "text": "1", "line": 1, "column": 5},'
                write(unit, '(a)') '  {"type": "eof", "text": "", "line": 1, "column": 6}'
                write(unit, '(a)') ']}'
                close(unit)
                iostat = 0
            end block

            if (iostat == 0) then
                ! Test --from-tokens option with wait flag for CI reliability
                call run_fortran_with_args('test_cli_json', '--from-tokens', &
                                          path_join(temp_dir, 'test_tokens.json'), iostat)

                if (iostat == 0) then
                    print *, '    PASS: --from-tokens executed successfully'
                else
              print *, '    FAIL: --from-tokens execution failed with exit code', iostat
                    test_json_functionality = .false.
                end if
            else
                print *, '    FAIL: Could not create test JSON tokens file'
                test_json_functionality = .false.
            end if
        end block

        ! Test --from-ast option
        print *, '  Testing --from-ast...'
        block
            integer :: iostat

            ! Create a simple JSON AST file
            block
                integer :: unit
                open(newunit=unit, file=path_join(temp_dir, 'test_ast.json'), status='replace')
                write(unit, '(a)') '{"type": "program", "name": "test"}'
                close(unit)
                iostat = 0
            end block

            if (iostat == 0) then
                ! Test --from-ast option with wait flag for CI reliability
                call run_fortran_with_args('test_cli_json_ast', '--from-ast', &
                                          path_join(temp_dir, 'test_ast.json'), iostat)

                if (iostat == 0) then
                    print *, '    PASS: --from-ast executed successfully'
                else
                 print *, '    FAIL: --from-ast execution failed with exit code', iostat
                    test_json_functionality = .false.
                end if
            else
                print *, '    FAIL: Could not create test JSON AST file'
                test_json_functionality = .false.
            end if
        end block

        ! Test --from-semantic option
        print *, '  Testing --from-semantic...'
        block
            integer :: iostat

            ! Create a simple JSON semantic file
            block
                integer :: unit
                open(newunit=unit, file=path_join(temp_dir, 'test_semantic.json'), status='replace')
                write(unit, '(a)') '{"annotated_ast": {"type": "program", "name": "test"}}'
                close(unit)
                iostat = 0
            end block

            if (iostat == 0) then
                ! Test --from-semantic option with wait flag for CI reliability
                call run_fortran_with_args('test_cli_json_sem', '--from-semantic', &
                                          path_join(temp_dir, 'test_semantic.json'), iostat)

                if (iostat == 0) then
                    print *, '    PASS: --from-semantic executed successfully'
                else
            print *, '    FAIL: --from-semantic execution failed with exit code', iostat
                    test_json_functionality = .false.
                end if
            else
                print *, '    FAIL: Could not create test JSON semantic file'
                test_json_functionality = .false.
            end if
        end block

        ! Clean up test files
        block
            integer :: iostat
            call execute_command_line('rm -f "'//path_join(temp_dir, 'test_*.json')//'" "'// &
                                       path_join(temp_dir, 'test_*.f90')//'"', &
                                      exitstat=iostat)
        end block

    end function test_json_functionality

end program test_cli_json_options
