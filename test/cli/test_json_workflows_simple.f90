program test_json_workflows_simple
    use temp_utils, only: get_system_temp_dir, create_temp_dir, get_project_root, create_test_cache_dir, path_join
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== JSON Workflow Tests (Simple) ==='
    print *

    ! Test basic JSON operations
    if (.not. test_tokens_json_creation()) all_passed = .false.
    if (.not. test_ast_json_creation()) all_passed = .false.
    if (.not. test_json_from_tokens()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All JSON workflow tests passed!'
        stop 0
    else
        print *, 'Some JSON workflow tests failed!'
        stop 1
    end if

contains

    logical function test_tokens_json_creation()
        character(len=:), allocatable :: temp_dir, cache_dir, test_file
        integer :: iostat, unit

        test_tokens_json_creation = .true.
        temp_dir = create_temp_dir('json_test_tokens')
        cache_dir = create_test_cache_dir('json_simple_basic')
        test_file = path_join(temp_dir, 'test_tokens.f')

        print *, 'Testing token JSON creation...'

        ! Create simple source in temp directory
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'x = 1'
        close (unit)

        ! Generate tokens JSON
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//temp_dir//'" && '// &
                           'cd "'//project_root//'" && fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                                      '"'//temp_dir//'/test_tokens.f" --debug-tokens'// &
                                      merge(' > nul 2>&1      ', ' > /dev/null 2>&1', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block

        if (iostat == 0) then
            ! Check if tokens JSON was created in temp dir
            inquire (file=path_join(temp_dir, 'test_tokens_tokens.json'), exist=test_tokens_json_creation)
            if (test_tokens_json_creation) then
                print *, '  PASS: Tokens JSON created'
            else
                print *, '  FAIL: Tokens JSON not found'
            end if
        else
            print *, '  FAIL: Token generation failed'
            test_tokens_json_creation = .false.
        end if

    end function test_tokens_json_creation

    logical function test_ast_json_creation()
        character(len=:), allocatable :: temp_dir, cache_dir, test_file
        integer :: iostat, unit

        test_ast_json_creation = .true.
        temp_dir = create_temp_dir('json_test_ast')
        cache_dir = create_test_cache_dir('json_simple_ast')
        test_file = path_join(temp_dir, 'test_ast.f')

        print *, 'Testing AST JSON creation...'

        ! Create simple source in temp directory
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'y = 2 * 3'
        close (unit)

        ! Generate AST JSON (through tokens first)
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//temp_dir//'" && '// &
                           'cd "'//project_root//'" && fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                                      '"'//temp_dir//'/test_ast.f" --debug-tokens'// &
                                      merge(' > nul 2>&1      ', ' > /dev/null 2>&1', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block

        if (iostat == 0) then
            ! Now parse tokens to AST
            block
                character(len=:), allocatable :: project_root
                project_root = get_project_root()
                call execute_command_line('cd "'//temp_dir//'" && '// &
                           'cd "'//project_root//'" && fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                    '"'//temp_dir//'/test_ast_tokens.json" --from-tokens --debug-ast'// &
                    merge(' > nul 2>&1      ', ' > /dev/null 2>&1', get_os_type() == OS_WINDOWS), &
                                          wait=.true., exitstat=iostat)
            end block

            if (iostat == 0) then
                inquire (file=path_join(temp_dir, 'test_ast_tokens_ast.json'), exist=test_ast_json_creation)
                if (test_ast_json_creation) then
                    print *, '  PASS: AST JSON created'
                else
                    print *, '  FAIL: AST JSON not found'
                end if
            else
                print *, '  FAIL: AST generation failed'
                test_ast_json_creation = .false.
            end if
        else
            print *, '  FAIL: Initial token generation failed'
            test_ast_json_creation = .false.
        end if

    end function test_ast_json_creation

    logical function test_json_from_tokens()
        character(len=:), allocatable :: temp_dir, cache_dir, test_file
        integer :: iostat, unit

        test_json_from_tokens = .true.
        temp_dir = create_temp_dir('json_test_direct')
        cache_dir = create_test_cache_dir('json_simple_tokens')
        test_file = path_join(temp_dir, 'direct_tokens.json')

        print *, 'Testing JSON from tokens workflow...'

        ! Create a tokens JSON file directly in temp directory
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') '{"tokens": ['
    write (unit, '(a)') '  {"type": "identifier", "text": "z", "line": 1, "column": 1},'
      write (unit, '(a)') '  {"type": "operator", "text": "=", "line": 1, "column": 3},'
       write (unit, '(a)') '  {"type": "number", "text": "99", "line": 1, "column": 5},'
        write (unit, '(a)') '  {"type": "eof", "text": "", "line": 1, "column": 7}'
        write (unit, '(a)') ']}'
        close (unit)

        ! Process tokens JSON
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//temp_dir//'" && '// &
                           'cd "'//project_root//'" && fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                                  '"'//temp_dir//'/direct_tokens.json" --from-tokens'// &
                                  merge(' > nul 2>&1      ', ' > /dev/null 2>&1', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block

        if (iostat == 0) then
            print *, '  PASS: JSON from tokens processed successfully'
        else
            print *, '  FAIL: JSON from tokens processing failed'
            test_json_from_tokens = .false.
        end if

        ! No manual cleanup needed - temp_dir will be cleaned automatically

    end function test_json_from_tokens

end program test_json_workflows_simple
