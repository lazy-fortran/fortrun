program test_frontend_parser_if_statement
    use lexer_core
    use ast_core
    use parser_core
    use parser_control_flow_module, only: parse_if
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for if statements
    if (.not. test_simple_if()) all_passed = .false.
    if (.not. test_if_then_else()) all_passed = .false.
    if (.not. test_if_elseif_else()) all_passed = .false.
    if (.not. test_nested_if()) all_passed = .false.
    if (.not. test_one_line_if()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All if statement parser tests passed"
        stop 0
    else
        print '(a)', "Some if statement parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_if()
        ! TDD Test 1: Parse simple if/then/endif
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_simple_if = .true.

        print '(a)', "Testing simple if/then/endif..."

        ! Test: if (x > 0) then
        call tokenize_statement("if (x > 0) then", tokens)
        stmt = parse_statement(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: No AST node returned for if statement"
            test_simple_if = .false.
        else
            ! Check what type of node we get
            select type (stmt)
            type is (if_node)
                print '(a)', "PASS: If statement parsed as if_node"

                ! Check condition exists
                if (.not. allocated(stmt%condition)) then
                    print '(a)', "FAIL: No condition in if node"
                    test_simple_if = .false.
                else
                    ! Check condition structure
                    select type (cond => stmt%condition)
                    type is (binary_op_node)
                        if (cond%operator == ">") then
                            print '(a)', "PASS: Condition operator is >"
                        else
                            print '(a)', "FAIL: Wrong condition operator"
                            test_simple_if = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Condition is not a binary_op_node"
                        test_simple_if = .false.
                    end select
                end if

                ! Check then body exists (even if empty for this test)
                if (.not. allocated(stmt%then_body)) then
                    print '(a)', "FAIL: No then_body in if node"
                    test_simple_if = .false.
                else
             print '(a,i0)', "INFO: then_body has ", size(stmt%then_body), " statements"
                end if
            type is (identifier_node)
                print '(a)', "FAIL: If statement parsed as identifier"
                test_simple_if = .false.
            class default
                print '(a)', "FAIL: If statement parsed as wrong node type"
                test_simple_if = .false.
            end select
        end if

    end function test_simple_if

    logical function test_if_then_else()
        ! TDD Test 2: Parse if/then/else/endif
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_if_then_else = .true.

        print '(a)', "Testing if/then/else/endif..."

        ! For now, just test the initial if statement
        call tokenize_statement("if (a == b) then", tokens)
        stmt = parse_statement(tokens)

        if (allocated(stmt)) then
            select type (stmt)
            type is (if_node)
                print '(a)', "PASS: If/else statement parsed as if_node"

                ! Check else body exists (we're only parsing the if line here)
                if (allocated(stmt%else_body)) then
             print '(a,i0)', "INFO: else_body has ", size(stmt%else_body), " statements"
                else
               print '(a)', "INFO: else_body not allocated (expected for partial parse)"
                end if
            class default
                print '(a)', "FAIL: If/else statement parsed as wrong node type"
                test_if_then_else = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for if/else statement"
            test_if_then_else = .false.
        end if

    end function test_if_then_else

    logical function test_if_elseif_else()
        ! TDD Test 3: Parse if/then/elseif/else/endif
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_if_elseif_else = .true.

        print '(a)', "Testing if/then/elseif/else/endif..."

        ! Test the initial if statement
        call tokenize_statement("if (x < 0) then", tokens)
        stmt = parse_statement(tokens)

        if (allocated(stmt)) then
            select type (stmt)
            type is (if_node)
                print '(a)', "PASS: If/elseif/else statement parsed as if_node"

                ! Check elseif blocks
                if (allocated(stmt%elseif_blocks)) then
                    print '(a,i0)', "INFO: ", size(stmt%elseif_blocks), " elseif blocks"
                else
                    print '(a)', "INFO: No elseif blocks (expected for partial parse)"
                end if
            class default
                print '(a)', "FAIL: If/elseif/else statement parsed as wrong node type"
                test_if_elseif_else = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for if/elseif/else statement"
            test_if_elseif_else = .false.
        end if

    end function test_if_elseif_else

    logical function test_nested_if()
        ! TDD Test 4: Parse nested if statements
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_nested_if = .true.

        print '(a)', "Testing nested if statements..."

        ! Test outer if
        call tokenize_statement("if (x > 0) then", tokens)
        stmt = parse_statement(tokens)

        if (allocated(stmt)) then
            select type (stmt)
            type is (if_node)
                print '(a)', "PASS: Nested if statement parsed as if_node"
            class default
                print '(a)', "FAIL: Nested if statement parsed as wrong node type"
                test_nested_if = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for nested if statement"
            test_nested_if = .false.
        end if

    end function test_nested_if

    logical function test_one_line_if()
        ! TDD Test 5: Parse one-line if statement
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_one_line_if = .true.

        print '(a)', "Testing one-line if statement..."

        ! Test: if (x > 0) y = 1
        call tokenize_statement("if (x > 0) y = 1", tokens)
        stmt = parse_statement(tokens)

        if (allocated(stmt)) then
            select type (stmt)
            type is (if_node)
                print '(a)', "PASS: One-line if statement parsed as if_node"

                ! Check condition
                if (allocated(stmt%condition)) then
                    select type (cond => stmt%condition)
                    type is (binary_op_node)
                        if (cond%operator == ">") then
                            print '(a)', "PASS: One-line if condition correct"
                        else
                            print '(a)', "FAIL: Wrong operator in one-line if"
                            test_one_line_if = .false.
                        end if
                    class default
                        print '(a)', "FAIL: One-line if condition not binary_op"
                        test_one_line_if = .false.
                    end select
                else
                    print '(a)', "FAIL: No condition in one-line if"
                    test_one_line_if = .false.
                end if

                ! Check then body has one statement
                if (allocated(stmt%then_body)) then
                    if (size(stmt%then_body) == 1) then
                        print '(a)', "PASS: One-line if has single statement"
                    else
           print '(a,i0)', "FAIL: One-line if has ", size(stmt%then_body), " statements"
                        test_one_line_if = .false.
                    end if
                else
                    print '(a)', "FAIL: No then_body in one-line if"
                    test_one_line_if = .false.
                end if
            class default
                print '(a)', "FAIL: One-line if statement parsed as wrong node type"
                test_one_line_if = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for one-line if statement"
            test_one_line_if = .false.
        end if

    end function test_one_line_if

    ! Helper to tokenize a statement and add EOF
    subroutine tokenize_statement(statement, tokens)
        character(len=*), intent(in) :: statement
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(token_t), allocatable :: temp_tokens(:)
        integer :: n

        call tokenize_core(statement, temp_tokens)
        n = size(temp_tokens)
        allocate (tokens(n + 1))
        tokens(1:n) = temp_tokens
        tokens(n + 1)%kind = TK_EOF
        tokens(n + 1)%text = ""
        tokens(n + 1)%line = 1
        tokens(n + 1)%column = len(statement) + 1
    end subroutine tokenize_statement

end program test_frontend_parser_if_statement
