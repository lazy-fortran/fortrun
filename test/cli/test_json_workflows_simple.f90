program test_json_workflows_simple
    use temp_utils, only: get_system_temp_dir
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
        character(len=:), allocatable :: temp_dir
        integer :: iostat, unit

        test_tokens_json_creation = .true.
        temp_dir = get_system_temp_dir()

        print *, 'Testing token JSON creation...'

        ! Clear cache first
        call execute_command_line('fpm run fortran -- --clear-cache > /dev/null 2>&1', wait=.true.)

        ! Create simple source
        open (newunit=unit, file='test_tokens.f', status='replace')
        write (unit, '(a)') 'x = 1'
        close (unit)

        ! Generate tokens JSON
        call execute_command_line('fpm run fortran -- test_tokens.f --debug-tokens > /dev/null 2>&1', &
                                  wait=.true., exitstat=iostat)

        if (iostat == 0) then
            ! Check if tokens JSON was created
            inquire (file='test_tokens_tokens.json', exist=test_tokens_json_creation)
            if (test_tokens_json_creation) then
                print *, '  PASS: Tokens JSON created'
                ! Clean up
   call execute_command_line('rm -f test_tokens.f test_tokens_tokens.json', wait=.true.)
            else
                print *, '  FAIL: Tokens JSON not found'
            end if
        else
            print *, '  FAIL: Token generation failed'
            test_tokens_json_creation = .false.
        end if

    end function test_tokens_json_creation

    logical function test_ast_json_creation()
        character(len=:), allocatable :: temp_dir
        integer :: iostat, unit

        test_ast_json_creation = .true.
        temp_dir = get_system_temp_dir()

        print *, 'Testing AST JSON creation...'

        ! Clear cache first
        call execute_command_line('fpm run fortran -- --clear-cache > /dev/null 2>&1', wait=.true.)

        ! Create simple source
        open (newunit=unit, file='test_ast.f', status='replace')
        write (unit, '(a)') 'y = 2 * 3'
        close (unit)

        ! Generate AST JSON (through tokens first)
        call execute_command_line('fpm run fortran -- test_ast.f --debug-tokens > /dev/null 2>&1', &
                                  wait=.true., exitstat=iostat)

        if (iostat == 0) then
            ! Now parse tokens to AST
            call execute_command_line('fpm run fortran -- test_ast_tokens.json --from-tokens --debug-ast > /dev/null 2>&1', &
                                      wait=.true., exitstat=iostat)

            if (iostat == 0) then
                inquire (file='test_ast_tokens_ast.json', exist=test_ast_json_creation)
                if (test_ast_json_creation) then
                    print *, '  PASS: AST JSON created'
                    ! Clean up
                    call execute_command_line('rm -f test_ast.f test_ast_tokens.json test_ast_tokens_ast.json', wait=.true.)
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
        character(len=:), allocatable :: temp_dir
        integer :: iostat, unit

        test_json_from_tokens = .true.
        temp_dir = get_system_temp_dir()

        print *, 'Testing JSON from tokens workflow...'

        ! Create a tokens JSON file directly
        open (newunit=unit, file='direct_tokens.json', status='replace')
        write (unit, '(a)') '{"tokens": ['
    write (unit, '(a)') '  {"type": "identifier", "text": "z", "line": 1, "column": 1},'
      write (unit, '(a)') '  {"type": "operator", "text": "=", "line": 1, "column": 3},'
       write (unit, '(a)') '  {"type": "number", "text": "99", "line": 1, "column": 5},'
        write (unit, '(a)') '  {"type": "eof", "text": "", "line": 1, "column": 7}'
        write (unit, '(a)') ']}'
        close (unit)

        ! Process tokens JSON
        call execute_command_line('fpm run fortran -- direct_tokens.json --from-tokens > /dev/null 2>&1', &
                                  wait=.true., exitstat=iostat)

        if (iostat == 0) then
            print *, '  PASS: JSON from tokens processed successfully'
        else
            print *, '  FAIL: JSON from tokens processing failed'
            test_json_from_tokens = .false.
        end if

        ! Clean up
    call execute_command_line('rm -f direct_tokens.json direct_tokens.f90', wait=.true.)

    end function test_json_from_tokens

end program test_json_workflows_simple
