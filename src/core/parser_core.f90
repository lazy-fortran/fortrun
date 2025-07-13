module parser_core
    use lexer_core
    use ast_core, only: ast_node, assignment_node, binary_op_node, identifier_node, &
                         literal_node, function_call_node, function_def_node, &
                         create_assignment, create_binary_op, create_identifier, &
                         create_literal, create_function_call, create_function_def, &
                         LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    implicit none
    private

    ! Parser state for tracking tokens
    type, public :: parser_state_t
        type(token_t), allocatable :: tokens(:)
        integer :: current_token = 1
    contains
        procedure :: peek => parser_peek
        procedure :: consume => parser_consume
        procedure :: is_at_end => parser_is_at_end
        procedure :: match => parser_match
    end type parser_state_t

    ! Public parsing interface
    public :: parse_expression, parse_statement
    public :: create_parser_state, parse_primary

contains

    ! Create parser state from tokens
    function create_parser_state(tokens) result(state)
        type(token_t), intent(in) :: tokens(:)
        type(parser_state_t) :: state
        
        state%tokens = tokens
        state%current_token = 1
    end function create_parser_state

    ! Peek at current token without consuming it
    function parser_peek(this) result(current_token)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current_token
        
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
        class(parser_state_t), intent(inout) :: this
        type(token_t) :: consumed_token
        
        consumed_token = this%peek()
        if (.not. this%is_at_end()) then
            this%current_token = this%current_token + 1
        end if
    end function parser_consume

    ! Check if we're at the end of tokens
    logical function parser_is_at_end(this)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current
        
        current = this%peek()
        parser_is_at_end = (current%kind == TK_EOF)
    end function parser_is_at_end

    ! Check if current token matches expected kind and consume if so
    logical function parser_match(this, expected_kind)
        class(parser_state_t), intent(inout) :: this
        integer, intent(in) :: expected_kind
        type(token_t) :: current, consumed
        
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
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: expr
        type(parser_state_t) :: parser
        
        parser = create_parser_state(tokens)
        expr = parse_comparison(parser)
    end function parse_expression
    
    ! Parse comparison operators (lowest precedence)
    function parse_comparison(parser) result(expr)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: right_expr, temp_expr
        type(token_t) :: op_token
        
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
                class(ast_node), allocatable :: args(:)
                type(token_t) :: paren
                integer :: arg_count
                
                next_token = parser%peek()
                if (next_token%kind == TK_OPERATOR .and. next_token%text == "(") then
                    ! Parse function call
                    
                    func_name = current%text
                    arg_count = 0
                    
                    ! Consume opening paren
                    paren = parser%consume()
                    
                    ! Parse arguments (simplified - only handles single argument for now)
                    next_token = parser%peek()
                    if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ")") then
                        block
                            class(ast_node), allocatable :: arg
                            arg = parse_primary(parser)
                            if (allocated(arg)) then
                                arg_count = 1
                                allocate(args(1), source=arg)
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
                        expr = create_function_call(func_name, args, current%line, current%column)
                    else
                        ! For empty args, use a dummy array
                        block
                            class(ast_node), allocatable :: empty_args(:)
                            allocate(identifier_node :: empty_args(0))
                            expr = create_function_call(func_name, empty_args, current%line, current%column)
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
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: stmt
        type(parser_state_t) :: parser
        type(token_t) :: id_token, op_token, first_token
        class(ast_node), allocatable :: target, value
        
        parser = create_parser_state(tokens)
        
        ! Check first token to determine statement type
        first_token = parser%peek()
        
        ! Check for function definition: [type] function name(params)
        if (first_token%kind == TK_KEYWORD .and. first_token%text == "function") then
            stmt = parse_function_definition(parser)
            return
        else if (first_token%kind == TK_KEYWORD .and. &
                 (first_token%text == "real" .or. first_token%text == "integer" .or. &
                  first_token%text == "logical" .or. first_token%text == "character")) then
            ! Look ahead to see if next token is "function"
            if (parser%current_token + 1 <= size(parser%tokens)) then
                block
                    type(token_t) :: second_token
                    second_token = parser%tokens(parser%current_token + 1)
                    if (second_token%kind == TK_KEYWORD .and. second_token%text == "function") then
                        stmt = parse_function_definition(parser)
                        return
                    end if
                end block
            end if
        end if
        
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
                block
                    type(assignment_node), allocatable :: assign_node
                    allocate(assign_node)
                    assign_node = create_assignment(target, value, id_token%line, id_token%column)
                    allocate(stmt, source=assign_node)
                end block
            else
                ! Not an assignment, treat as expression statement
                stmt = create_identifier(id_token%text, id_token%line, id_token%column)
            end if
        else
            ! Parse as expression
            stmt = parse_expression(tokens)
        end if
        
    end function parse_statement
    
    ! Parse function definition: [type] function name(params)
    function parse_function_definition(parser) result(func_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: func_node
        
        character(len=:), allocatable :: return_type_str, func_name
        type(token_t) :: token
        class(ast_node), allocatable :: params(:), body(:), return_type
        integer :: line, column
        
        ! Initialize
        return_type_str = ""
        
        ! Check if we have a return type
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. &
            (token%text == "real" .or. token%text == "integer" .or. &
             token%text == "logical" .or. token%text == "character")) then
            return_type_str = token%text
            token = parser%consume()
        end if
        
        ! Expect "function" keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            ! Error - not a function definition
            func_node = create_literal("! Error: Expected function keyword", LITERAL_STRING, 1, 1)
            return
        end if
        
        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            func_name = token%text
            token = parser%consume()
        else
            ! Error - missing function name
            func_node = create_literal("! Error: Missing function name", LITERAL_STRING, 1, 1)
            return
        end if
        
        ! For now, skip parameter parsing and create a simple function node
        ! This is a minimal implementation for TDD
        allocate(identifier_node :: params(0))  ! Empty parameters for now
        allocate(identifier_node :: body(0))    ! Empty body for now
        
        ! Create return type node from string
        if (len_trim(return_type_str) > 0) then
            return_type = create_identifier(return_type_str, line, column)
        else
            return_type = create_identifier("", line, column)
        end if
        
        ! Create function definition node
        func_node = create_function_def(func_name, params, return_type, body, line, column)
        
    end function parse_function_definition

end module parser_core