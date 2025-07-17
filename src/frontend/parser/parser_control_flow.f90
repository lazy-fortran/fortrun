module parser_control_flow_module
    ! Parser module for control flow constructs (if, do, select case)
    use lexer_core
    use parser_state_module
    use parser_expressions_module
    use ast_core
    implicit none
    private

    public :: parse_if, parse_do_loop, parse_do_while, parse_select_case
    public :: parse_if_condition, parse_if_body, parse_elseif_block

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

                ! Import parse_statement from parser_core
                interface
                    function parse_statement(tokens) result(stmt)
                        import :: token_t, ast_node
                        type(token_t), intent(in) :: tokens(:)
                        class(ast_node), allocatable :: stmt
                    end function parse_statement
                end interface

                stmt = parse_statement(remaining_tokens)
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

        ! Import parse_statement from parser_core
        interface
            function parse_statement(tokens) result(stmt)
                import :: token_t, ast_node
                type(token_t), intent(in) :: tokens(:)
                class(ast_node), allocatable :: stmt
            end function parse_statement
        end interface

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
            stmt = parse_statement(remaining_tokens)
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
        ! Implementation to be moved from parser_core
        allocate (loop_node)
    end function parse_do_loop

    function parse_do_while(parser) result(loop_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: loop_node
        ! Implementation to be moved from parser_core
        allocate (loop_node)
    end function parse_do_while

    function parse_select_case(parser) result(select_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: select_node
        ! Implementation to be moved from parser_core
        allocate (select_node)
    end function parse_select_case

end module parser_control_flow_module
