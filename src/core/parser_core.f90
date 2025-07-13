module parser_core
    use lexer_core
    use ast_core
    implicit none
    private

    ! Parser state for tracking tokens
    type, public :: parser_state
        type(token), allocatable :: tokens(:)
        integer :: current_token = 1
    contains
        procedure :: peek => parser_peek
        procedure :: consume => parser_consume
        procedure :: is_at_end => parser_is_at_end
        procedure :: match => parser_match
    end type parser_state

    ! Public parsing interface
    public :: parse_expression, parse_statement
    public :: create_parser_state, parse_primary

contains

    ! Create parser state from tokens
    function create_parser_state(tokens) result(state)
        type(token), intent(in) :: tokens(:)
        type(parser_state) :: state
        
        state%tokens = tokens
        state%current_token = 1
    end function create_parser_state

    ! Peek at current token without consuming it
    function parser_peek(this) result(current_token)
        class(parser_state), intent(in) :: this
        type(token) :: current_token
        
        if (this%current_token <= size(this%tokens)) then
            current_token = this%tokens(this%current_token)
        else
            ! Return EOF token
            current_token%kind = TK_EOF
            current_token%text = ""
            current_token%line = 1
            current_token%column = 1
        end if
    end function parser_peek

    ! Consume current token and advance
    function parser_consume(this) result(consumed_token)
        class(parser_state), intent(inout) :: this
        type(token) :: consumed_token
        
        consumed_token = this%peek()
        if (.not. this%is_at_end()) then
            this%current_token = this%current_token + 1
        end if
    end function parser_consume

    ! Check if we're at the end of tokens
    logical function parser_is_at_end(this)
        class(parser_state), intent(in) :: this
        type(token) :: current
        
        current = this%peek()
        parser_is_at_end = (current%kind == TK_EOF)
    end function parser_is_at_end

    ! Check if current token matches expected kind and consume if so
    logical function parser_match(this, expected_kind)
        class(parser_state), intent(inout) :: this
        integer, intent(in) :: expected_kind
        type(token) :: current, consumed
        
        current = this%peek()
        if (current%kind == expected_kind) then
            consumed = this%consume()
            parser_match = .true.
        else
            parser_match = .false.
        end if
    end function parser_match

    ! Parse expression with operator precedence
    function parse_expression(tokens) result(expr)
        type(token), intent(in) :: tokens(:)
        class(ast_node), allocatable :: expr
        type(parser_state) :: parser
        
        parser = create_parser_state(tokens)
        expr = parse_comparison(parser)
    end function parse_expression
    
    ! Parse comparison operators (lowest precedence)
    function parse_comparison(parser) result(expr)
        type(parser_state), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token) :: op_token
        
        expr = parse_term(parser)
        
        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "==" .or. op_token%text == "/=" .or. &
                 op_token%text == "<=" .or. op_token%text == ">=" .or. &
                 op_token%text == "<" .or. op_token%text == ">")) then
                op_token = parser%consume()
                right_expr = parse_term(parser)
                temp_expr = create_binary_op(expr, right_expr, op_token%text, op_token%line, op_token%column)
                call move_alloc(temp_expr, expr)
            else
                exit
            end if
        end do
    end function parse_comparison
    
    ! Parse addition and subtraction
    function parse_term(parser) result(expr)
        type(parser_state), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token) :: op_token
        
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
        type(parser_state), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token) :: op_token
        
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
    function parse_primary(parser) result(expr)
        type(parser_state), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        type(token) :: current
        
        current = parser%peek()
        
        select case (current%kind)
        case (TK_NUMBER)
            ! Parse number literal
            current = parser%consume()
            expr = create_literal(current%text, LITERAL_INTEGER, current%line, current%column)
            
        case (TK_IDENTIFIER)
            ! Parse identifier
            current = parser%consume()
            expr = create_identifier(current%text, current%line, current%column)
            
        case (TK_OPERATOR)
            ! Check for parentheses
            if (current%text == "(") then
                current = parser%consume()  ! consume '('
                expr = parse_comparison(parser)  ! parse the expression inside
                current = parser%peek()
                if (current%text == ")") then
                    current = parser%consume()  ! consume ')'
                end if
            else
                ! Fallback for unrecognized operators
                expr = create_literal("0", LITERAL_INTEGER, 1, 1)
            end if
            
        case default
            ! Fallback
            expr = create_literal("0", LITERAL_INTEGER, 1, 1)
        end select
    end function parse_primary

    ! Parse a simple statement (minimal implementation for TDD)
    function parse_statement(tokens) result(stmt)
        type(token), intent(in) :: tokens(:)
        class(ast_node), allocatable :: stmt
        type(parser_state) :: parser
        type(token) :: id_token, op_token
        class(ast_node), allocatable :: target, value
        
        parser = create_parser_state(tokens)
        
        ! Look for pattern: IDENTIFIER = EXPRESSION
        id_token = parser%peek()
        if (id_token%kind == TK_IDENTIFIER) then
            id_token = parser%consume()
            
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                op_token = parser%consume()
                
                ! Create target identifier
                target = create_identifier(id_token%text, id_token%line, id_token%column)
                
                ! Parse value expression
                value = parse_expression(parser%tokens(parser%current_token:))
                
                ! Create assignment
                stmt = create_assignment(target, value, id_token%line, id_token%column)
            else
                ! Not an assignment, treat as expression statement
                stmt = create_identifier(id_token%text, id_token%line, id_token%column)
            end if
        else
            ! Parse as expression
            stmt = parse_expression(tokens)
        end if
        
    end function parse_statement

end module parser_core