module parser_core
    use lexer_core
    use ast_core, only: ast_node, ast_node_wrapper, assignment_node, binary_op_node, identifier_node, &
                         literal_node, function_call_node, function_def_node, print_statement_node, &
                         use_statement_node, &
                         create_assignment, create_binary_op, create_identifier, &
                         create_literal, create_function_call, create_function_def, create_print_statement, &
                         create_use_statement, &
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
                            arg = parse_primary(parser)
                            if (allocated(arg)) then
                                arg_count = 1
                                allocate(args(1))
                                allocate(args(1)%node, source=arg)
                                
                                ! Parse additional arguments separated by commas
                                do
                                    next_token = parser%peek()
                                    if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ",") exit
                                    
                                    ! Consume comma
                                    next_token = parser%consume()
                                    
                                    ! Parse next argument
                                    arg = parse_primary(parser)
                                    if (allocated(arg)) then
                                        ! Extend wrapper array: args = [args, new_wrapper]
                                        block
                                            type(ast_node_wrapper) :: new_wrapper
                                            allocate(new_wrapper%node, source=arg)
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
                        expr = create_function_call(func_name, args, current%line, current%column)
                    else
                        ! For empty args, create empty function call
                        block
                            type(ast_node_wrapper), allocatable :: empty_args(:)
                            allocate(empty_args(0))  ! Empty wrapper array
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
                expr = create_literal("! Unrecognized operator", LITERAL_STRING, current%line, current%column)
            end if
            
        case default
            ! Fallback
            expr = create_literal("! Unrecognized token", LITERAL_STRING, current%line, current%column)
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
        
        ! Check for use statement
        if (first_token%kind == TK_KEYWORD .and. first_token%text == "use") then
            stmt = parse_use_statement(parser)
            return
        ! Check for print statement
        else if (first_token%kind == TK_KEYWORD .and. first_token%text == "print") then
            stmt = parse_print_statement(parser)
            return
        ! Check for function definition: [type] function name(params)
        else if (first_token%kind == TK_KEYWORD .and. first_token%text == "function") then
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
        
        ! Look for pattern: IDENTIFIER = EXPRESSION or IDENTIFIER(params) = EXPRESSION
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
    
    ! Parse print statement: print format_spec, arg1, arg2, ...
    function parse_print_statement(parser) result(print_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: print_node
        
        type(token_t) :: token
        type(ast_node_wrapper), allocatable :: wrapper_args(:)
        integer :: line, column, arg_count
        character(len=:), allocatable :: format_spec
        
        ! Consume 'print' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Parse format spec (*, format string, or format variable)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            format_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_STRING) then
            ! Format string like '(a,f5.1,a,f5.1,a,f5.1)'
            format_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            ! Format variable
            format_spec = token%text
            token = parser%consume()
        else
            format_spec = "*"  ! Default
        end if
        
        ! Skip comma after format spec if present
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
        end if
        
        ! Parse all print arguments using wrapper pattern with array extension syntax
        arg_count = 0
        if (.not. parser%is_at_end()) then
            block
                class(ast_node), allocatable :: current_arg
                
                ! Parse first argument
                current_arg = parse_comparison(parser)
                if (allocated(current_arg)) then
                    arg_count = 1
                    allocate(wrapper_args(1))
                    allocate(wrapper_args(1)%node, source=current_arg)
                    
                    ! Parse additional arguments separated by commas
                    do
                        token = parser%peek()
                        if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit
                        
                        ! Consume comma
                        token = parser%consume()
                        
                                ! Parse next argument
                        current_arg = parse_comparison(parser)
                        if (allocated(current_arg)) then
                            ! Extend wrapper array using [array, new_element] syntax with temporary
                            block
                                type(ast_node_wrapper) :: new_wrapper
                                allocate(new_wrapper%node, source=current_arg)
                                wrapper_args = [wrapper_args, new_wrapper]
                                arg_count = arg_count + 1
                            end block
                        else
                            exit
                        end if
                    end do
                end if
            end block
        end if
        
        ! Create print statement node with wrapper args
        block
            type(print_statement_node) :: print_stmt
            if (allocated(wrapper_args)) then
                print_stmt%format_spec = format_spec
                print_stmt%args = wrapper_args
            else
                print_stmt%format_spec = format_spec
                allocate(print_stmt%args(0))  ! Empty wrapper array
            end if
            print_stmt%line = line
            print_stmt%column = column
            allocate(print_node, source=print_stmt)
        end block
        
    end function parse_print_statement

    ! Parse function definition: [type] function name(params) ... end function
    function parse_function_definition(parser) result(func_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: func_node
        
        character(len=:), allocatable :: return_type_str, func_name
        type(token_t) :: token
        type(ast_node_wrapper), allocatable :: params(:), body(:)
        class(ast_node), allocatable :: return_type
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
        
        ! Parse parameters (basic implementation)
        allocate(params(0))  ! Empty parameters for now
        
        ! Look for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            
            ! Skip parameter parsing for now - just find closing paren
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                    exit
                end if
                token = parser%consume()
            end do
        end if
        
        ! Parse function body (collect all statements until "end function")
        block
            type(ast_node_wrapper), allocatable :: body_statements(:)
            class(ast_node), allocatable :: stmt
            integer :: body_count, i
            
            body_count = 0
            
            ! Parse statements until we hit "end function"
            do while (.not. parser%is_at_end())
                token = parser%peek()
                
                ! Check for "end function"
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Look ahead for "function"
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        block
                            type(token_t) :: next_token
                            next_token = parser%tokens(parser%current_token + 1)
                            if (next_token%kind == TK_KEYWORD .and. next_token%text == "function") then
                                ! Consume "end function"
                                token = parser%consume()
                                token = parser%consume()
                                exit
                            end if
                        end block
                    end if
                end if
                
                ! Parse the statement (simplified - just create a literal for now)
                stmt = create_literal("! Function body statement", LITERAL_STRING, token%line, token%column)
                
                ! Extend body array using wrapper pattern
                block
                    type(ast_node_wrapper) :: new_wrapper
                    allocate(new_wrapper%node, source=stmt)
                    if (allocated(body_statements)) then
                        body_statements = [body_statements, new_wrapper]
                    else
                        body_statements = [new_wrapper]
                    end if
                    body_count = body_count + 1
                end block
                
                ! Skip to next token
                token = parser%consume()
            end do
            
            ! Convert wrapper array to body array
            if (body_count > 0) then
                allocate(body(body_count))
                do i = 1, body_count
                    allocate(body(i)%node, source=body_statements(i)%node)
                end do
                deallocate(body_statements)
            else
                allocate(body(0))  ! Empty body
            end if
        end block
        
        ! Create return type node from string
        if (len_trim(return_type_str) > 0) then
            return_type = create_identifier(return_type_str, line, column)
        else
            return_type = create_identifier("", line, column)
        end if
        
        ! Create function definition node
        func_node = create_function_def(func_name, params, return_type, body, line, column)
        
    end function parse_function_definition

    ! Parse use statement: use module_name [, only: ...]
    function parse_use_statement(parser) result(stmt)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: stmt
        type(token_t) :: token
        character(len=:), allocatable :: module_name
        
        ! Consume 'use' keyword
        token = parser%consume()
        
        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
        else
            ! Invalid use statement - return placeholder
            stmt = create_literal("! Invalid use statement", LITERAL_STRING, token%line, token%column)
            return
        end if
        
        ! For now, ignore 'only' clause
        ! TODO: Parse 'only' clause when needed
        
        ! Create use statement node (without only list for now)
        stmt = create_use_statement(module_name, line=token%line, column=token%column)
        
    end function parse_use_statement

end module parser_core