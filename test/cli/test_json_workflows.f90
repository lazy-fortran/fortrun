program test_json_workflows
    use temp_utils, only: get_system_temp_dir
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== JSON Workflow Tests ==='
    print *

    ! Test complete JSON pipeline workflows
    if (.not. test_simple_assignment_workflow()) all_passed = .false.
    if (.not. test_function_workflow()) all_passed = .false.
    if (.not. test_control_flow_workflow()) all_passed = .false.
    if (.not. test_round_trip_workflow()) all_passed = .false.

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

    logical function test_simple_assignment_workflow()
        character(len=:), allocatable :: temp_dir
        integer :: iostat, unit
        logical :: success

        test_simple_assignment_workflow = .true.
        temp_dir = get_system_temp_dir()

        print *, 'Testing simple assignment workflow...'

        ! Step 1: Create source file
        open (newunit=unit, file=temp_dir//'/simple.f', status='replace')
        write (unit, '(a)') 'x = 42'
        close (unit)

        ! Step 2: Generate tokens
        call execute_command_line('fpm run fortran -- '//temp_dir//'/simple.f --debug-tokens 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Token generation failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! Step 3: Parse to AST from tokens
        call execute_command_line('fpm run fortran -- '//temp_dir//'/simple_tokens.json --from-tokens --debug-ast 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: AST generation from tokens failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! Step 4: Semantic analysis from AST
        call execute_command_line('fpm run fortran -- '//temp_dir//'/simple_ast.json --from-ast --debug-semantic 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Semantic analysis from AST failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! Step 5: Code generation from semantic AST
        call execute_command_line('fpm run fortran -- '//temp_dir//'/simple_ast_typed.json --from-ast > '// &
                   temp_dir//'/generated.f90 2>/dev/null', wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Code generation from semantic AST failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! For now, just verify JSON files were created
        ! (Full round-trip not implemented yet)
        if (verify_file_exists(temp_dir//'/simple_tokens.json') .and. &
            verify_file_exists(temp_dir//'/simple_ast.json')) then
            print *, '  PASS: Simple assignment workflow (JSON files created)'
        else
            print *, '  FAIL: JSON files not created'
            test_simple_assignment_workflow = .false.
        end if

    end function test_simple_assignment_workflow

    logical function test_function_workflow()
        character(len=:), allocatable :: temp_dir
        integer :: iostat, unit

        test_function_workflow = .true.
        temp_dir = get_system_temp_dir()

        print *, 'Testing function workflow...'

        ! Create source with function
        open (newunit=unit, file=temp_dir//'/func.f', status='replace')
        write (unit, '(a)') 'real function square(x)'
        write (unit, '(a)') '  real :: x'
        write (unit, '(a)') '  square = x * x'
        write (unit, '(a)') 'end function'
        close (unit)

        ! Run full pipeline
        call execute_command_line('fpm run fortran -- '//temp_dir//'/func.f --debug-tokens 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Token generation for function failed'
            test_function_workflow = .false.
            return
        end if

        call execute_command_line('fpm run fortran -- '//temp_dir//'/func_tokens.json --from-tokens --debug-ast 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: AST generation for function failed'
            test_function_workflow = .false.
            return
        end if

        call execute_command_line('fpm run fortran -- '//temp_dir//'/func_ast.json --from-ast --debug-semantic 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Semantic analysis for function failed'
            test_function_workflow = .false.
            return
        end if

        ! Verify function in AST output
        if (verify_file_contains(temp_dir//'/func_ast.json', '"name": "square"')) then
            print *, '  PASS: Function workflow'
        else
            print *, '  FAIL: Function not found in AST output'
            test_function_workflow = .false.
        end if

    end function test_function_workflow

    logical function test_control_flow_workflow()
        character(len=:), allocatable :: temp_dir
        integer :: iostat, unit

        test_control_flow_workflow = .true.
        temp_dir = get_system_temp_dir()

        print *, 'Testing control flow workflow...'

        ! Create source with if statement in a program
        open (newunit=unit, file=temp_dir//'/if.f', status='replace')
        write (unit, '(a)') 'x = 5'
        write (unit, '(a)') 'if (x > 0) then'
        write (unit, '(a)') '  y = 1'
        write (unit, '(a)') 'else'
        write (unit, '(a)') '  y = -1'
        write (unit, '(a)') 'end if'
        close (unit)

        ! Generate tokens
        call execute_command_line('fpm run fortran -- '//temp_dir//'/if.f --debug-tokens 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Token generation for control flow failed'
            test_control_flow_workflow = .false.
            return
        end if

        ! Parse to AST
        call execute_command_line('fpm run fortran -- '//temp_dir//'/if_tokens.json --from-tokens --debug-ast 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: AST generation for control flow failed'
            test_control_flow_workflow = .false.
            return
        end if

        ! Verify if node in AST
        if (verify_file_contains(temp_dir//'/if_ast.json', '"type": "if"')) then
            print *, '  PASS: Control flow workflow'
        else
            print *, '  FAIL: If statement not found in AST'
            test_control_flow_workflow = .false.
        end if

    end function test_control_flow_workflow

    logical function test_round_trip_workflow()
        character(len=:), allocatable :: temp_dir
        integer :: iostat, unit
        character(len=1024) :: line
        logical :: found_assignment

        test_round_trip_workflow = .true.
        temp_dir = get_system_temp_dir()

        print *, 'Testing round-trip workflow...'

        ! Create original source
        open (newunit=unit, file=temp_dir//'/original.f', status='replace')
        write (unit, '(a)') 'x = 10'
        write (unit, '(a)') 'y = x + 5'
        write (unit, '(a)') 'print *, y'
        close (unit)

        ! Full pipeline: source -> tokens -> AST -> semantic -> code
        call execute_command_line('fpm run fortran -- '//temp_dir//'/original.f --debug-tokens 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Initial tokenization failed'
            test_round_trip_workflow = .false.
            return
        end if

        call execute_command_line('fpm run fortran -- '//temp_dir//'/original_tokens.json --from-tokens --debug-ast 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: AST generation in round-trip failed'
            test_round_trip_workflow = .false.
            return
        end if

        call execute_command_line('fpm run fortran -- '//temp_dir//'/original_ast.json --from-ast --debug-semantic 2>/dev/null', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Semantic analysis in round-trip failed'
            test_round_trip_workflow = .false.
            return
        end if

        call execute_command_line('fpm run fortran -- '//temp_dir//'/original_ast_typed.json --from-ast > '// &
                   temp_dir//'/roundtrip.f90 2>/dev/null', wait=.true., exitstat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Code generation in round-trip failed'
            test_round_trip_workflow = .false.
            return
        end if

        ! Just verify that all JSON files were created in the pipeline
        if (verify_file_exists(temp_dir//'/original_tokens.json') .and. &
            verify_file_exists(temp_dir//'/original_ast.json')) then
            print *, '  PASS: Round-trip workflow (JSON pipeline working)'
        else
            print *, '  FAIL: JSON pipeline incomplete'
            test_round_trip_workflow = .false.
        end if

    end function test_round_trip_workflow

    ! Helper function to verify file contains string
    logical function verify_file_contains(filename, search_string)
        character(len=*), intent(in) :: filename, search_string
        integer :: unit, iostat
        character(len=1024) :: line

        verify_file_contains = .false.

        open (newunit=unit, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, search_string) > 0) then
                verify_file_contains = .true.
                exit
            end if
        end do

        close (unit)
    end function verify_file_contains

    ! Helper function to check if file exists
    logical function verify_file_exists(filename)
        character(len=*), intent(in) :: filename
        logical :: exists

        inquire (file=filename, exist=exists)
        verify_file_exists = exists
    end function verify_file_exists

end program test_json_workflows
