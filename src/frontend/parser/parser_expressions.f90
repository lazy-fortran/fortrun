module parser_expressions_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_NUMBER, TK_STRING, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD
    use ast_core
    use parser_state_module, only: parser_state_t, create_parser_state
    implicit none
    private

    ! Public expression parsing interface
    public :: parse_expression
    public :: parse_logical_or, parse_logical_and, parse_comparison
    public :: parse_member_access, parse_term, parse_factor, parse_primary

contains

    ! Main expression parsing entry point
    function parse_expression(tokens) result(expr)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: expr
        type(parser_state_t) :: parser

        parser = create_parser_state(tokens)
        expr = parse_logical_or(parser)
    end function parse_expression

    ! Parse logical OR operators (lowest precedence)
    function parse_logical_or(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token_t) :: op_token

        expr = parse_logical_and(parser)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".or.") then
                op_token = parser%consume()  ! consume operator
                right_expr = parse_logical_and(parser)
                if (allocated(right_expr)) then
                    allocate (temp_expr, source=expr)
                    expr = create_binary_op(temp_expr, right_expr, op_token%text)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function parse_logical_or

    ! Parse logical AND operators
    function parse_logical_and(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token_t) :: op_token

        expr = parse_comparison(parser)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".and.") then
                op_token = parser%consume()  ! consume operator
                right_expr = parse_comparison(parser)
                if (allocated(right_expr)) then
                    allocate (temp_expr, source=expr)
                    expr = create_binary_op(temp_expr, right_expr, op_token%text)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function parse_logical_and

    ! Parse comparison operators
    function parse_comparison(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token_t) :: op_token

        expr = parse_member_access(parser)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "==" .or. op_token%text == "/=" .or. &
                 op_token%text == "<=" .or. op_token%text == ">=" .or. &
                 op_token%text == "<" .or. op_token%text == ">")) then
                op_token = parser%consume()
                right_expr = parse_member_access(parser)
                temp_expr = create_binary_op(expr, right_expr, op_token%text, op_token%line, op_token%column)
                call move_alloc(temp_expr, expr)
            else
                exit
            end if
        end do
    end function parse_comparison

    ! Parse member access operator (%)
    function parse_member_access(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token_t) :: op_token

        expr = parse_term(parser)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "%") then
                op_token = parser%consume()
                right_expr = parse_term(parser)
                temp_expr = create_binary_op(expr, right_expr, op_token%text, op_token%line, op_token%column)
                call move_alloc(temp_expr, expr)
            else
                exit
            end if
        end do
    end function parse_member_access

    ! Parse addition and subtraction
    function parse_term(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token_t) :: op_token

        expr = parse_factor(parser)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "+" .or. op_token%text == "-")) then
                op_token = parser%consume()
                right_expr = parse_factor(parser)
                temp_expr = create_binary_op(expr, right_expr, op_token%text, op_token%line, op_token%column)
                call move_alloc(temp_expr, expr)
            else
                exit
            end if
        end do
    end function parse_term

    ! Parse multiplication, division, and power
    function parse_factor(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token_t) :: op_token

        expr = parse_primary(parser)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
       (op_token%text == "*" .or. op_token%text == "/" .or. op_token%text == "**")) then
                op_token = parser%consume()
                right_expr = parse_primary(parser)
                temp_expr = create_binary_op(expr, right_expr, op_token%text, op_token%line, op_token%column)
                call move_alloc(temp_expr, expr)
            else
                exit
            end if
        end do
    end function parse_factor

    ! Parse primary expressions (literals, identifiers, parentheses)
    recursive function parse_primary(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        type(token_t) :: current

        current = parser%peek()

        select case (current%kind)
        case (TK_NUMBER)
            ! Parse number literal
            current = parser%consume()
            if (index(current%text, '.') > 0) then
                ! Contains decimal point - classify as real
         expr = create_literal(current%text, LITERAL_REAL, current%line, current%column)
            else
                ! No decimal point - classify as integer
      expr = create_literal(current%text, LITERAL_INTEGER, current%line, current%column)
            end if

        case (TK_STRING)
            ! Parse string literal
            current = parser%consume()
       expr = create_literal(current%text, LITERAL_STRING, current%line, current%column)

        case (TK_IDENTIFIER)
            ! Parse identifier or function call
            current = parser%consume()

            ! Check if followed by '(' for function call
            block
                type(token_t) :: next_token
                character(len=:), allocatable :: func_name
                type(ast_node_wrapper), allocatable :: args(:)
                type(token_t) :: paren
                integer :: arg_count

                next_token = parser%peek()
                if (next_token%kind == TK_OPERATOR .and. next_token%text == "(") then
                    ! Parse function call

                    func_name = current%text
                    arg_count = 0

                    ! Consume opening paren
                    paren = parser%consume()

                    ! Parse arguments (now handles multiple arguments)
                    next_token = parser%peek()
                    if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ")") then
                        block
                            class(ast_node), allocatable :: arg

                            ! Handle multiple arguments using wrapper pattern
                            arg_count = 0

                            ! Parse first argument
                            arg = parse_comparison(parser)
                            if (allocated(arg)) then
                                arg_count = 1
                                allocate (args(1))
                                allocate (args(1)%node, source=arg)

                                ! Parse additional arguments separated by commas
                                do
                                    next_token = parser%peek()
                    if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ",") exit

                                    ! Consume comma
                                    next_token = parser%consume()

                                    ! Parse next argument
                                    arg = parse_comparison(parser)
                                    if (allocated(arg)) then
                                        ! Extend wrapper array: args = [args, new_wrapper]
                                        block
                                            type(ast_node_wrapper) :: new_wrapper
                                            allocate (new_wrapper%node, source=arg)
                                            args = [args, new_wrapper]  ! Extend array with wrapper
                                            arg_count = arg_count + 1
                                        end block
                                    else
                                        exit
                                    end if
                                end do
                            end if
                        end block
                    end if

                    ! Consume closing paren if present
                    next_token = parser%peek()
                   if (next_token%kind == TK_OPERATOR .and. next_token%text == ")") then
                        paren = parser%consume()
                    end if

                    ! Create function call node
                    if (allocated(args)) then
          expr = create_call_or_subscript(func_name, args, current%line, current%column)
                    else
                        ! For empty args, create empty function call
                        block
                            type(ast_node_wrapper), allocatable :: empty_args(:)
                            allocate (empty_args(0))  ! Empty wrapper array
    expr = create_call_or_subscript(func_name, empty_args, current%line, current%column)
                        end block
                    end if
                else
                    expr = create_identifier(current%text, current%line, current%column)
                end if
            end block

        case (TK_OPERATOR)
            ! Check for parentheses
            if (current%text == "(") then
                current = parser%consume()  ! consume '('
                expr = parse_comparison(parser)  ! parse the expression inside
                current = parser%peek()
                if (current%text == ")") then
                    current = parser%consume()  ! consume ')'
                end if
            else if (current%text == "-" .or. current%text == "+") then
                ! Unary operator
                block
                    type(token_t) :: op_token
                    class(ast_node), allocatable :: operand
                    op_token = parser%consume()
                    operand = parse_primary(parser)
                    if (allocated(operand)) then
                        ! Create unary expression as binary op with zero
                        if (op_token%text == "-") then
                            ! For unary minus, create 0 - operand
                            block
                                class(ast_node), allocatable :: zero
             zero = create_literal("0", LITERAL_INTEGER, op_token%line, op_token%column)
                                expr = create_binary_op(zero, operand, "-")
                            end block
                        else
                            ! For unary plus, just return the operand
                            allocate (expr, source=operand)
                        end if
                    end if
                end block
            else if (current%text == ".not.") then
                ! Logical NOT operator
                block
                    type(token_t) :: op_token
                    class(ast_node), allocatable :: operand
                    op_token = parser%consume()
                    operand = parse_primary(parser)
                    if (allocated(operand)) then
                        ! Create unary NOT expression as binary op with false
                        block
                            class(ast_node), allocatable :: false_literal
             false_literal = create_literal(".false.", LITERAL_LOGICAL, op_token%line, op_token%column)
                            expr = create_binary_op(false_literal, operand, ".not.")
                        end block
                    end if
                end block
            else if (current%text == ".") then
                ! Check for logical literals (.true. or .false.)
                block
                    type(token_t) :: next_token, third_token
                    if (parser%current_token + 2 <= size(parser%tokens)) then
                        next_token = parser%tokens(parser%current_token + 1)
                        third_token = parser%tokens(parser%current_token + 2)

                        if (next_token%kind == TK_IDENTIFIER .and. &
                     third_token%kind == TK_OPERATOR .and. third_token%text == ".") then
                     if (next_token%text == "true" .or. next_token%text == "false") then
                                ! It's a logical literal
                                current = parser%consume()  ! consume first '.'
                                current = parser%consume()  ! consume 'true'/'false'
                                current = parser%consume()  ! consume second '.'
               expr = create_literal("."//trim(next_token%text)//".", LITERAL_LOGICAL, &
                                                      current%line, current%column)
                            else
                                ! Not a logical literal
                 expr = create_literal("", LITERAL_STRING, current%line, current%column)
                                current = parser%consume()
                            end if
                        else
                            ! Not a logical literal pattern
                 expr = create_literal("", LITERAL_STRING, current%line, current%column)
                            current = parser%consume()
                        end if
                    else
                        ! Not enough tokens
                 expr = create_literal("", LITERAL_STRING, current%line, current%column)
                        current = parser%consume()
                    end if
                end block
            else
                ! Unrecognized operator - create a placeholder
                expr = create_literal("", LITERAL_STRING, current%line, current%column)
                current = parser%consume()
            end if

        case (TK_KEYWORD)
            ! Handle logical constants
            current = parser%consume()
            if (current%text == ".true." .or. current%text == ".false.") then
      expr = create_literal(current%text, LITERAL_LOGICAL, current%line, current%column)
            else
                ! Other keywords - create placeholder for now
                expr = create_literal("", LITERAL_STRING, current%line, current%column)
            end if

        case default
            ! Unrecognized token - create a placeholder and skip
            expr = create_literal("", LITERAL_STRING, current%line, current%column)
            current = parser%consume()
        end select
    end function parse_primary

end module parser_expressions_module
