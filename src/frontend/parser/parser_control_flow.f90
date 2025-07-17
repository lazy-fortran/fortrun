module parser_control_flow_module
    ! Parser module for control flow constructs (if, do, select case)
    use lexer_core
    use parser_state_module
  use parser_expressions_module, only: parse_primary, parse_expression, parse_logical_or
    use ast_core
    implicit none
    private

    public :: parse_if, parse_do_loop, parse_do_while, parse_select_case
    public :: parse_if_condition, parse_if_body, parse_elseif_block
    public :: parse_do_while_from_do, parse_basic_statement

contains

    ! Parse if statement
    function parse_if(parser) result(if_stmt)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: if_stmt

        type(token_t) :: if_token, then_token
        class(ast_node), allocatable :: condition
        type(ast_node_wrapper), allocatable :: then_body(:), else_body(:)
        type(elseif_wrapper), allocatable :: elseif_blocks(:)
        integer :: elseif_count

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (should be in parentheses for standard if/then/endif)
        condition = parse_if_condition(parser)

        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. then_token%text == "then") then
            ! Standard if/then/endif block
            then_token = parser%consume()

            ! Parse then body statements
            then_body = parse_if_body(parser)

            ! Check for elseif/else blocks
            elseif_count = 0
            allocate (elseif_blocks(0))

            do while (.not. parser%is_at_end())
                then_token = parser%peek()

                if (then_token%kind == TK_KEYWORD) then
                 if (then_token%text == "elseif" .or. then_token%text == "else if") then
                        ! Parse elseif block
                        elseif_count = elseif_count + 1
                        elseif_blocks = [elseif_blocks, parse_elseif_block(parser)]
                    else if (then_token%text == "else") then
                        ! Parse else block
                        then_token = parser%consume()  ! consume 'else'
                        else_body = parse_if_body(parser)
                        exit
              else if (then_token%text == "endif" .or. then_token%text == "end if") then
                        ! End of if statement
                        then_token = parser%consume()
                        exit
                    else
                        ! Other statement, stop parsing if block
                        exit
                    end if
                else
                    ! Not a keyword, continue parsing body
                    exit
                end if
            end do

            ! Create if node
            if_stmt = create_if(condition, then_body, elseif_blocks, else_body, &
                                if_token%line, if_token%column)
        else
            ! One-line if statement (no then keyword)
            allocate (then_body(1))

            ! Parse the single statement
            block
                class(ast_node), allocatable :: stmt
                type(token_t), allocatable :: remaining_tokens(:)
                integer :: i, n

                ! Count remaining tokens
                n = 0
                do i = parser%current_token, size(parser%tokens)
                    n = n + 1
                end do

                ! Extract remaining tokens
                allocate (remaining_tokens(n))
                remaining_tokens = parser%tokens(parser%current_token:)

                ! Use parse_basic_statement instead of parse_statement

                stmt = parse_basic_statement(remaining_tokens)
                if (allocated(stmt)) then
                    allocate (then_body(1)%node, source=stmt)
                end if
            end block

            ! Create if node with no elseif/else blocks
            allocate (elseif_blocks(0))
            allocate (else_body(0))
            if_stmt = create_if(condition, then_body, elseif_blocks, else_body, &
                                if_token%line, if_token%column)
        end if

    end function parse_if

    ! Parse if condition (handles parentheses if present)
    function parse_if_condition(parser) result(condition)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: condition
        type(token_t) :: paren_token
        type(token_t), allocatable :: remaining_tokens(:)
        integer :: i, n

        ! Check for opening parenthesis
        paren_token = parser%peek()
        if (paren_token%kind == TK_OPERATOR .and. paren_token%text == "(") then
            paren_token = parser%consume()  ! consume '('

            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            ! Parse the condition expression
            condition = parse_expression(remaining_tokens)

            ! Consume closing parenthesis
            paren_token = parser%peek()
            if (paren_token%kind == TK_OPERATOR .and. paren_token%text == ")") then
                paren_token = parser%consume()
            end if
        else
            ! No parentheses, just parse the expression
            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            condition = parse_expression(remaining_tokens)
        end if

    end function parse_if_condition

    ! Parse if/elseif/else body statements
    function parse_if_body(parser) result(body)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_wrapper), allocatable :: body(:)
        type(token_t) :: token
        type(token_t), allocatable :: remaining_tokens(:)
        integer :: stmt_count, i, n
        class(ast_node), allocatable :: stmt

        allocate (body(0))
        stmt_count = 0

        ! Use parse_basic_statement instead of parse_statement

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of body
            if (token%kind == TK_KEYWORD) then
                if (token%text == "elseif" .or. token%text == "else if" .or. &
                    token%text == "else" .or. token%text == "endif" .or. &
                    token%text == "end if") then
                    exit
                end if
            end if

            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            ! Parse statement
            stmt = parse_basic_statement(remaining_tokens)
            if (allocated(stmt)) then
                stmt_count = stmt_count + 1
                block
                    type(ast_node_wrapper) :: wrapper
                    allocate (wrapper%node, source=stmt)
                    body = [body, wrapper]
                end block
            end if

            deallocate (remaining_tokens)
        end do

    end function parse_if_body

    ! Parse elseif block
    function parse_elseif_block(parser) result(elseif_block)
        type(parser_state_t), intent(inout) :: parser
        type(elseif_wrapper) :: elseif_block
        type(token_t) :: elseif_token

        ! Consume 'elseif' or 'else if'
        elseif_token = parser%consume()

        ! Parse condition
        elseif_block%condition = parse_if_condition(parser)

        ! Look for 'then' keyword
        elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. elseif_token%text == "then") then
            elseif_token = parser%consume()
        end if

        ! Parse body
        elseif_block%body = parse_if_body(parser)

    end function parse_elseif_block

    ! Add other control flow parsing functions here...
    ! Due to length, I'll create placeholders that can be filled with the actual implementations

    function parse_do_loop(parser) result(loop_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: loop_node

        type(token_t) :: do_token, var_token, eq_token, comma_token
        character(len=:), allocatable :: var_name
        class(ast_node), allocatable :: start_expr, end_expr, step_expr
        integer :: line, column

        ! Starting to parse do loop

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        ! Check if it's a do while loop
        var_token = parser%peek()
        if (var_token%kind == TK_KEYWORD .and. var_token%text == "while") then
            ! Parse as do while loop
            loop_node = parse_do_while_from_do(parser, line, column)
            return
        end if

        ! Get variable name
        var_token = parser%consume()
        if (var_token%kind /= TK_IDENTIFIER) then
            ! Error: expected identifier
            ! ERROR - expected identifier
            return
        end if
        var_name = var_token%text
        ! Got variable name

        ! Expect '='
        eq_token = parser%consume()
        if (eq_token%kind /= TK_OPERATOR .or. eq_token%text /= "=") then
            ! Error: expected '='
            return
        end if

        ! Parse start expression (simplified - just parse next token as literal)
        start_expr = parse_primary(parser)

        ! Expect ','
        comma_token = parser%consume()
        if (comma_token%kind /= TK_OPERATOR .or. comma_token%text /= ",") then
            ! Error: expected ','
            return
        end if

        ! Parse end expression
        end_expr = parse_primary(parser)

        ! Check for optional step
        if (.not. parser%is_at_end()) then
            comma_token = parser%peek()
            if (comma_token%kind == TK_OPERATOR .and. comma_token%text == ",") then
                comma_token = parser%consume()  ! consume comma
                step_expr = parse_primary(parser)
            end if
        end if

        ! Parse body statements until 'end do'
        block
            type(ast_node_wrapper), allocatable :: body_statements(:), temp_body(:)
            class(ast_node), allocatable :: stmt
            integer :: body_count, stmt_start, stmt_end, j
            type(token_t), allocatable :: stmt_tokens(:)
            type(do_loop_node), allocatable :: do_node

            body_count = 0

            ! Parse body statements
            do while (parser%current_token <= size(parser%tokens))
                ! Check for 'end do'
                block
                    type(token_t) :: current_token
                    current_token = parser%peek()

            if (current_token%kind == TK_KEYWORD .and. current_token%text == "end") then
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                              parser%tokens(parser%current_token + 1)%text == "do") then
                                ! Found 'end do', consume both tokens and exit
                                current_token = parser%consume()  ! consume 'end'
                                current_token = parser%consume()  ! consume 'do'
                                exit
                            end if
                        end if
                    end if
                end block

                ! Parse statement until end of line
                stmt_start = parser%current_token
                stmt_end = stmt_start

                ! Find end of current statement (same line)
                do j = stmt_start, size(parser%tokens)
                    if (parser%tokens(j)%kind == TK_EOF) then
                        stmt_end = j
                        exit
                    end if
   if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) then
                        stmt_end = j - 1
                        exit
                    end if
                    stmt_end = j
                end do

                ! Extract statement tokens
                if (stmt_end >= stmt_start) then
                    allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                    ! Add EOF token
                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                    stmt_tokens(stmt_end - stmt_start + 2)%text = ""
              stmt_tokens(stmt_end - stmt_start + 2)%line = parser%tokens(stmt_end)%line
      stmt_tokens(stmt_end - stmt_start + 2)%column = parser%tokens(stmt_end)%column + 1

                    ! Parse the statement
                    stmt = parse_basic_statement(stmt_tokens)

                    ! Add to body if successfully parsed
                    if (allocated(stmt)) then
                        block
                            type(ast_node_wrapper) :: new_wrapper
                            allocate (new_wrapper%node, source=stmt)
                            if (allocated(body_statements)) then
                                body_statements = [body_statements, new_wrapper]
                            else
                                body_statements = [new_wrapper]
                            end if
                            body_count = body_count + 1
                        end block
                    end if

                    deallocate (stmt_tokens)
                end if

                ! Move to next statement
                parser%current_token = stmt_end + 1
            end do

            ! Create do loop node with body
            allocate (do_node)
            if (allocated(step_expr)) then
                do_node = create_do_loop(var_name, start_expr, end_expr, step_expr, line=line, column=column)
            else
      do_node = create_do_loop(var_name, start_expr, end_expr, line=line, column=column)
            end if

            ! Set body if we have statements
            if (body_count > 0) then
                allocate (do_node%body(body_count))
                do j = 1, body_count
                    allocate (do_node%body(j)%node, source=body_statements(j)%node)
                end do
            end if

            allocate (loop_node, source=do_node)
            ! Successfully created do loop node
        end block

    end function parse_do_loop

    function parse_do_while(parser) result(loop_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: loop_node

        type(token_t) :: do_token
        integer :: line, column

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        loop_node = parse_do_while_from_do(parser, line, column)
    end function parse_do_while

    function parse_select_case(parser) result(select_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: select_node

        type(token_t) :: select_token, case_token, lparen_token, rparen_token
        class(ast_node), allocatable :: expr
        type(case_wrapper), allocatable :: cases(:), temp_cases(:)
        integer :: case_count, line, column

        ! Consume 'select'
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect 'case'
        case_token = parser%consume()
        if (case_token%kind /= TK_KEYWORD .or. case_token%text /= "case") then
            ! Error: expected 'case' after 'select'
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '(' after 'select case'
            return
        end if

        ! Parse expression to match
        expr = parse_expression(parser%tokens(parser%current_token:))
        if (.not. allocated(expr)) then
            ! Error: expected expression in select case
            return
        end if

        ! Advance parser past the expression
        ! For now, assume expression consumes tokens until ')'
        do while (parser%current_token <= size(parser%tokens))
            rparen_token = parser%peek()
            if (rparen_token%kind == TK_OPERATOR .and. rparen_token%text == ")") then
                rparen_token = parser%consume()
                exit
            end if
            parser%current_token = parser%current_token + 1
        end do

        ! Parse case blocks
        allocate (cases(0))
        case_count = 0

        do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()

            if (case_token%kind == TK_KEYWORD) then
                if (case_token%text == "case") then
                    ! Parse case block
                    block
                        type(case_wrapper) :: new_case
                        type(token_t) :: value_token
                        class(ast_node), allocatable :: case_value

                        case_token = parser%consume()  ! consume 'case'

                        ! Check for default case
                        value_token = parser%peek()
            if (value_token%kind == TK_KEYWORD .and. value_token%text == "default") then
                            value_token = parser%consume()  ! consume 'default'
                            new_case%case_type = "case_default"
                        else
                            new_case%case_type = "case"

                            ! Expect '('
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == "(") then
                                value_token = parser%consume()  ! consume '('

                                ! Parse case value
                                case_value = parse_primary(parser)
                                if (allocated(case_value)) then
                                    allocate (new_case%value, source=case_value)
                                end if

                                ! Expect ')'
                                value_token = parser%peek()
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == ")") then
                                    value_token = parser%consume()  ! consume ')'
                                end if
                            end if
                        end if

                        ! Parse case body (for now, empty)
                        allocate (new_case%body(0))

                        ! Add to cases array
                        case_count = case_count + 1
                        if (case_count == 1) then
                            deallocate (cases)
                            allocate (cases(1))
                        else
                            allocate (temp_cases(case_count - 1))
                            temp_cases = cases(1:case_count - 1)
                            deallocate (cases)
                            allocate (cases(case_count))
                            cases(1:case_count - 1) = temp_cases
                            deallocate (temp_cases)
                        end if
                        cases(case_count) = new_case
                    end block
                else if (case_token%text == "end") then
                    ! Check for 'end select'
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                          parser%tokens(parser%current_token + 1)%text == "select") then
                            ! Found 'end select', consume both tokens and exit
                            case_token = parser%consume()  ! consume 'end'
                            case_token = parser%consume()  ! consume 'select'
                            exit
                        end if
                    end if
                else
                    ! Other keyword, skip
                    parser%current_token = parser%current_token + 1
                end if
            else
                ! Not a keyword, skip
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select case node
        if (case_count > 0) then
            select_node = create_select_case(expr, cases(1:case_count), line, column)
        else
            select_node = create_select_case(expr, line=line, column=column)
        end if

        if (allocated(cases)) deallocate (cases)
    end function parse_select_case

    ! Helper function to parse basic statements (simplified version)
    function parse_basic_statement(tokens) result(stmt)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: stmt
        type(parser_state_t) :: parser
        type(token_t) :: first_token

        parser = create_parser_state(tokens)
        first_token = parser%peek()

        ! Simple assignment statement: identifier = expression
        if (first_token%kind == TK_IDENTIFIER) then
            block
                type(token_t) :: id_token, op_token
                class(ast_node), allocatable :: target, value

                id_token = parser%consume()
                op_token = parser%peek()

                if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                    op_token = parser%consume()  ! consume '='
               target = create_identifier(id_token%text, id_token%line, id_token%column)
                    ! Get remaining tokens for expression parsing
                    block
                        type(token_t), allocatable :: expr_tokens(:)
                        integer :: remaining_count
                        remaining_count = size(tokens) - parser%current_token + 1
                        if (remaining_count > 0) then
                            allocate (expr_tokens(remaining_count))
                            expr_tokens = tokens(parser%current_token:)
                            value = parse_expression(expr_tokens)
                            if (allocated(value)) then
                 stmt = create_assignment(target, value, id_token%line, id_token%column)
                            end if
                        end if
                    end block
                end if
            end block
        end if

        ! If we couldn't parse it, create a placeholder
        if (.not. allocated(stmt)) then
            stmt = create_literal("! Unparsed statement", LITERAL_STRING, first_token%line, first_token%column)
        end if

    end function parse_basic_statement

    ! Helper function for parsing do while from do token
    function parse_do_while_from_do(parser, line, column) result(loop_node)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(in) :: line, column
        class(ast_node), allocatable :: loop_node

        type(token_t) :: while_token, lparen_token, rparen_token
        class(ast_node), allocatable :: condition

        ! Consume 'while'
        while_token = parser%consume()
        if (while_token%kind /= TK_KEYWORD .or. while_token%text /= "while") then
            ! Error: expected 'while'
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '('
            return
        end if

        ! Parse condition
        condition = parse_logical_or(parser)

        ! Expect ')'
        rparen_token = parser%consume()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            ! Error: expected ')'
            return
        end if

        ! Parse body statements until 'end do'
        block
            type(ast_node_wrapper), allocatable :: body_statements(:), temp_body(:)
            class(ast_node), allocatable :: stmt
            integer :: body_count, stmt_start, stmt_end, j
            type(token_t), allocatable :: stmt_tokens(:)
            type(do_while_node), allocatable :: while_node

            body_count = 0

            ! Parse body statements
            ! Starting do while body parsing
            do while (parser%current_token <= size(parser%tokens))
                ! Check for 'end do'
                block
                    type(token_t) :: current_token
                    current_token = parser%peek()

            if (current_token%kind == TK_KEYWORD .and. current_token%text == "end") then
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                              parser%tokens(parser%current_token + 1)%text == "do") then
                                ! Found 'end do', consume both tokens and exit
                                while_token = parser%consume()  ! consume 'end'
                                while_token = parser%consume()  ! consume 'do'
                                exit
                            end if
                        end if
                    end if
                end block

                ! Parse a statement
                stmt_start = parser%current_token
                stmt_end = stmt_start

                ! Find end of statement (next line or EOF)
                do j = stmt_start, size(parser%tokens)
   if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) exit
                    if (parser%tokens(j)%kind == TK_EOF) exit
                    stmt_end = j
                end do

                if (stmt_end >= stmt_start) then
                    allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                    stmt_tokens(stmt_end - stmt_start + 2)%text = ""

                    stmt = parse_basic_statement(stmt_tokens)

                    if (allocated(stmt)) then
                        if (body_count == 0) then
                            allocate (body_statements(1))
                        else
                            allocate (temp_body(body_count))
                            temp_body = body_statements(1:body_count)
                            deallocate (body_statements)
                            allocate (body_statements(body_count + 1))
                            body_statements(1:body_count) = temp_body
                            deallocate (temp_body)
                        end if
                        body_count = body_count + 1
                        allocate (body_statements(body_count)%node, source=stmt)
                    end if

                    deallocate (stmt_tokens)
                end if

                parser%current_token = stmt_end + 1
            end do

            ! Create do while node
            allocate (while_node)

            ! Pass body statements if available
            if (body_count > 0 .and. allocated(body_statements)) then
                ! Set body after creation
                while_node = create_do_while(condition, line=line, column=column)
                allocate (while_node%body(body_count))
                do j = 1, body_count
                    allocate (while_node%body(j)%node, source=body_statements(j)%node)
                end do
            else
                while_node = create_do_while(condition, line=line, column=column)
            end if

            allocate (loop_node, source=while_node)

            if (allocated(body_statements)) deallocate (body_statements)
        end block

    end function parse_do_while_from_do

end module parser_control_flow_module
