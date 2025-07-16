module parser_core
    use lexer_core
    use ast_core, only: ast_node, ast_node_wrapper, assignment_node, binary_op_node, identifier_node, &
            literal_node, function_call_node, function_def_node, print_statement_node, &
                         use_statement_node, declaration_node, do_loop_node, do_while_node, select_case_node, case_wrapper, &
            derived_type_node, create_assignment, create_binary_op, create_identifier, &
    create_literal, create_function_call, create_function_def, create_print_statement, &
                         create_use_statement, create_declaration, create_do_loop, create_do_while, create_select_case, &
     create_derived_type, LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
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
    public :: create_parser_state, parse_primary, parse_function_definition
    public :: parse_do_loop, parse_do_while, parse_select_case

contains

    ! Create parser state from tokens
    function create_parser_state(tokens) result(state)
        type(token_t), intent(in) :: tokens(:)
        type(parser_state_t) :: state

        allocate (state%tokens(size(tokens)))
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
                                allocate (args(1))
                                allocate (args(1)%node, source=arg)

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
              expr = create_function_call(func_name, args, current%line, current%column)
                    else
                        ! For empty args, create empty function call
                        block
                            type(ast_node_wrapper), allocatable :: empty_args(:)
                            allocate (empty_args(0))  ! Empty wrapper array
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

        case default
            ! Unrecognized token - create a placeholder and skip
            expr = create_literal("", LITERAL_STRING, current%line, current%column)
            current = parser%consume()
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
            ! Check for derived type definition vs variable declaration with derived type
        else if (first_token%kind == TK_KEYWORD .and. first_token%text == "type") then
            ! Check if this is a derived type definition or variable declaration
            block
                type(token_t) :: second_token
                logical :: is_derived_type_def

                is_derived_type_def = .false.

                ! Look at second token to determine
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    second_token = parser%tokens(parser%current_token + 1)

                    ! If second token is :: or identifier, it's a derived type definition
              if (second_token%kind == TK_OPERATOR .and. second_token%text == "::") then
                        is_derived_type_def = .true.
                    else if (second_token%kind == TK_IDENTIFIER) then
                        is_derived_type_def = .true.
                    end if
                end if

                if (is_derived_type_def) then
                    stmt = parse_derived_type(parser)
                    return
                else
                    ! This is a variable declaration like type(point) :: p
                    ! Check if it has ::
                    block
                        integer :: i
                        logical :: has_double_colon
                        has_double_colon = .false.

    do i = parser%current_token + 1, min(parser%current_token + 10, size(parser%tokens))
      if (parser%tokens(i)%kind == TK_OPERATOR .and. parser%tokens(i)%text == "::") then
                                has_double_colon = .true.
                                exit
 else if (parser%tokens(i)%kind == TK_KEYWORD .or. parser%tokens(i)%kind == TK_EOF) then
                                exit
                            end if
                        end do

                        if (has_double_colon) then
                            stmt = parse_declaration(parser)
                            return
                        end if
                    end block
                end if
            end block

            ! Variable declarations for other types (real :: x, integer :: i, etc.)
        else if (first_token%kind == TK_KEYWORD .and. &
                 (first_token%text == "real" .or. first_token%text == "integer" .or. &
               first_token%text == "logical" .or. first_token%text == "character" .or. &
                  first_token%text == "complex") .and. &
                 parser%current_token + 1 <= size(parser%tokens)) then
            ! Check if it's a declaration (has ::)
            block
                integer :: i
                logical :: has_double_colon
                has_double_colon = .false.

    do i = parser%current_token + 1, min(parser%current_token + 10, size(parser%tokens))
      if (parser%tokens(i)%kind == TK_OPERATOR .and. parser%tokens(i)%text == "::") then
                        has_double_colon = .true.
                        exit
 else if (parser%tokens(i)%kind == TK_KEYWORD .or. parser%tokens(i)%kind == TK_EOF) then
                        exit
                    end if
                end do

                if (has_double_colon) then
                    stmt = parse_declaration(parser)
                    return
                end if
            end block
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
            ! Check for do loop
        else if (first_token%kind == TK_KEYWORD .and. first_token%text == "do") then
            stmt = parse_do_loop(parser)
            return
            ! Check for select case
        else if (first_token%kind == TK_KEYWORD .and. first_token%text == "select") then
            stmt = parse_select_case(parser)
            return
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
                    allocate (assign_node)
          assign_node = create_assignment(target, value, id_token%line, id_token%column)
                    allocate (stmt, source=assign_node)
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

    ! Parse declaration statement: type :: variable [= expression]
    function parse_declaration(parser) result(decl_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: decl_node

        type(token_t) :: type_token, var_token
        character(len=:), allocatable :: type_name, var_name
        integer :: line, column, kind_value
        logical :: has_kind, is_array, is_allocatable
        type(ast_node_wrapper), allocatable :: dimensions(:)

        ! Get type name (real, integer, etc.)
        type_token = parser%consume()
        type_name = type_token%text
        line = type_token%line
        column = type_token%column
        has_kind = .false.
        kind_value = 0
        is_allocatable = .false.

        ! Check for kind specification (e.g., real(8)) or derived type (e.g., type(point))
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
            ! Consume '('
            type_token = parser%consume()

            ! Get what's inside the parentheses
            var_token = parser%peek()
            if (var_token%kind == TK_NUMBER) then
                ! This is a kind specification like real(8)
                var_token = parser%consume()
                read (var_token%text, *) kind_value
                has_kind = .true.

                ! Consume ')'
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                    type_token = parser%consume()
                else
                    ! Error: expected )
                    block
                        type(literal_node), allocatable :: error_node
                        allocate (error_node)
          error_node = create_literal("ERROR: Expected )", LITERAL_STRING, line, column)
                        allocate (decl_node, source=error_node)
                    end block
                    return
                end if
            else if (var_token%kind == TK_IDENTIFIER) then
                ! This could be a derived type like type(point) or character(len=20)
                block
                    character(len=:), allocatable :: type_spec
                    type_spec = var_token%text
                    var_token = parser%consume()

                    ! Check for more complex type specifications like len=20
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == "=") then
                        ! Handle len=20 syntax
                        var_token = parser%consume()  ! consume '='
                        type_spec = type_spec//"="

                        var_token = parser%peek()
                        if (var_token%kind == TK_NUMBER) then
                            var_token = parser%consume()
                            type_spec = type_spec//var_token%text
                        end if
                    end if

                    type_name = type_name//"("//type_spec//")"
                end block

                ! Consume ')'
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                    type_token = parser%consume()
                else
                    ! Error: expected )
                    block
                        type(literal_node), allocatable :: error_node
                        allocate (error_node)
          error_node = create_literal("ERROR: Expected )", LITERAL_STRING, line, column)
                        allocate (decl_node, source=error_node)
                    end block
                    return
                end if
            else
                ! Error: expected number or identifier
                block
                    type(literal_node), allocatable :: error_node
                    allocate (error_node)
 error_node = create_literal("ERROR: Expected kind value or type name", LITERAL_STRING, line, column)
                    allocate (decl_node, source=error_node)
                end block
                return
            end if
        end if

        ! Check for attributes like allocatable (e.g., "real, allocatable :: arr")
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
            ! Consume ','
            var_token = parser%consume()

            ! Parse attribute (for now, just handle allocatable)
            var_token = parser%peek()
         if (var_token%kind == TK_IDENTIFIER .and. var_token%text == "allocatable") then
                is_allocatable = .true.
                var_token = parser%consume()
            else
                ! Unknown attribute - for now, just consume it
                var_token = parser%consume()
            end if
        end if

        ! Consume ::
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "::") then
            type_token = parser%consume()
        else
            ! Error: expected ::
            block
                type(literal_node), allocatable :: error_node
                allocate (error_node)
         error_node = create_literal("ERROR: Expected ::", LITERAL_STRING, line, column)
                allocate (decl_node, source=error_node)
            end block
            return
        end if

        ! Get variable name
        var_token = parser%peek()
        if (var_token%kind == TK_IDENTIFIER) then
            var_token = parser%consume()
            var_name = var_token%text
        else
            ! Error: expected identifier
            block
                type(literal_node), allocatable :: error_node
                allocate (error_node)
 error_node = create_literal("ERROR: Expected identifier", LITERAL_STRING, line, column)
                allocate (decl_node, source=error_node)
            end block
            return
        end if

        ! Check for array dimensions (e.g., (10), (:), (1:10))
        is_array = .false.
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
            is_array = .true.
            ! Consume '('
            var_token = parser%consume()

            ! Parse dimensions - for now, just handle simple cases
            call parse_array_dimensions(parser, dimensions)

            ! Consume ')'
            var_token = parser%peek()
            if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                var_token = parser%consume()
            else
                ! Error: expected )
                block
                    type(literal_node), allocatable :: error_node
                    allocate (error_node)
    error_node = create_literal("ERROR: Expected ) after array dimensions", LITERAL_STRING, line, column)
                    allocate (decl_node, source=error_node)
                end block
                return
            end if
        end if

        ! Check for initialization (= expression)
        block
            class(ast_node), allocatable :: initializer
            var_token = parser%peek()
            if (var_token%kind == TK_OPERATOR .and. var_token%text == "=") then
                ! Consume '='
                var_token = parser%consume()

                ! Parse the initializer expression
                initializer = parse_comparison(parser)

                ! Store the initializer in the declaration node (will be handled in create_declaration)
                if (allocated(initializer)) then
                    ! Create declaration node with initializer
                    block
                        type(declaration_node), allocatable :: node
                        allocate (node)
                        if (has_kind .and. is_array) then
                            node = create_declaration(type_name, var_name, kind_value=kind_value, initializer=initializer, dimensions=dimensions, is_allocatable=is_allocatable, line=line, column=column)
                        else if (has_kind) then
                            node = create_declaration(type_name, var_name, kind_value=kind_value, initializer=initializer, is_allocatable=is_allocatable, line=line, column=column)
                        else if (is_array) then
                            node = create_declaration(type_name, var_name, initializer=initializer, dimensions=dimensions, is_allocatable=is_allocatable, line=line, column=column)
                        else
                            node = create_declaration(type_name, var_name, initializer=initializer, is_allocatable=is_allocatable, line=line, column=column)
                        end if
                        allocate (decl_node, source=node)
                    end block
                    return
                end if
            end if

            ! Consume any remaining tokens
            do while (.not. parser%is_at_end())
                var_token = parser%peek()
                if (var_token%kind == TK_EOF) exit
                var_token = parser%consume()
            end do
        end block

        ! Create declaration node
        block
            type(declaration_node), allocatable :: node
            allocate (node)
            if (has_kind .and. is_array) then
                node = create_declaration(type_name, var_name, kind_value=kind_value, dimensions=dimensions, is_allocatable=is_allocatable, line=line, column=column)
            else if (has_kind) then
                node = create_declaration(type_name, var_name, kind_value=kind_value, is_allocatable=is_allocatable, line=line, column=column)
            else if (is_array) then
                node = create_declaration(type_name, var_name, dimensions=dimensions, is_allocatable=is_allocatable, line=line, column=column)
            else
                node = create_declaration(type_name, var_name, is_allocatable=is_allocatable, line=line, column=column)
            end if
            allocate (decl_node, source=node)
        end block

    end function parse_declaration

    ! Parse array dimensions inside parentheses
    subroutine parse_array_dimensions(parser, dimensions)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_wrapper), allocatable, intent(out) :: dimensions(:)

        type(token_t) :: token
        type(ast_node_wrapper), allocatable :: temp_dims(:)
        integer :: dim_count

        ! Initialize
        dim_count = 0
        allocate (temp_dims(10))  ! Initial allocation

        ! Parse dimensions separated by commas
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                ! End of dimensions
                exit
            end if

            dim_count = dim_count + 1
            if (dim_count > size(temp_dims)) then
                ! Need to resize
                block
                    type(ast_node_wrapper), allocatable :: new_dims(:)
                    allocate (new_dims(size(temp_dims)*2))
                    new_dims(1:size(temp_dims)) = temp_dims
                    call move_alloc(new_dims, temp_dims)
                end block
            end if

            ! Parse a single dimension
            if (token%kind == TK_OPERATOR .and. token%text == ":") then
                ! Assumed shape dimension (:)
                token = parser%consume()
        allocate (temp_dims(dim_count)%node, source=create_literal(":", LITERAL_STRING))
            else if (token%kind == TK_NUMBER) then
                ! Could be fixed size dimension (10) or bounds notation (1:10)
                block
                    character(len=:), allocatable :: first_number
                    type(token_t) :: next_token

                    first_number = token%text
                    token = parser%consume()

                    ! Check if followed by colon for bounds notation
                    next_token = parser%peek()
                   if (next_token%kind == TK_OPERATOR .and. next_token%text == ":") then
                        ! Bounds notation like 1:10
                        token = parser%consume()  ! consume ':'
                        next_token = parser%peek()
                        if (next_token%kind == TK_NUMBER) then
                            ! Complete bounds notation
                            token = parser%consume()
                            allocate (temp_dims(dim_count)%node, source=create_literal(first_number//":"//token%text, LITERAL_STRING))
                        else
                            ! Just first_number:
                            allocate (temp_dims(dim_count)%node, source=create_literal(first_number//":", LITERAL_STRING))
                        end if
                    else
                        ! Just a fixed size dimension
                        allocate (temp_dims(dim_count)%node, source=create_literal(first_number, LITERAL_INTEGER))
                    end if
                end block
            else
                ! More complex dimension expressions - for now, just consume as identifier
                token = parser%consume()
 allocate (temp_dims(dim_count)%node, source=create_literal(token%text, LITERAL_STRING))
            end if

            ! Check for comma (multiple dimensions)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! Consume comma
                cycle
            else
                exit
            end if
        end do

        ! Copy to result
        if (dim_count > 0) then
            allocate (dimensions(dim_count))
            dimensions(1:dim_count) = temp_dims(1:dim_count)
        end if

    end subroutine parse_array_dimensions

    ! Parse derived type definition: type :: type_name or type(params) :: type_name
    function parse_derived_type(parser) result(type_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: type_node

        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: line, column
        logical :: has_parameters
        type(ast_node_wrapper), allocatable :: parameters(:)

        ! Consume 'type' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        has_parameters = .false.

        ! Check for :: or just get type name
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            ! Consume ::
            token = parser%consume()

            ! Get type name
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                type_name = token%text

                ! Check for parameters after type name: type :: matrix(n, m)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    ! Parse parameters
                    has_parameters = .true.
                    token = parser%consume()  ! consume '('
                    call parse_derived_type_parameters(parser, parameters)

                    ! Consume ')'
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()
                    end if
                end if
            else
                type_name = "unnamed_type"
            end if
        else if (token%kind == TK_IDENTIFIER) then
            ! Direct type name (like "type point")
            token = parser%consume()
            type_name = token%text

            ! Check for parameters after type name: type matrix(n, m)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                ! Parse parameters
                has_parameters = .true.
                token = parser%consume()  ! consume '('
                call parse_derived_type_parameters(parser, parameters)

                ! Consume ')'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        else
            type_name = "unnamed_type"
        end if

        ! Create derived type node
        block
            type(derived_type_node), allocatable :: node
            allocate (node)
            if (has_parameters) then
  node = create_derived_type(type_name, parameters=parameters, line=line, column=column)
            else
                node = create_derived_type(type_name, line=line, column=column)
            end if
            allocate (type_node, source=node)
        end block

    end function parse_derived_type

    ! Parse derived type parameters inside parentheses
    subroutine parse_derived_type_parameters(parser, parameters)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_wrapper), allocatable, intent(out) :: parameters(:)

        type(token_t) :: token
        type(ast_node_wrapper), allocatable :: temp_params(:)
        integer :: param_count

        ! Initialize
        param_count = 0
        allocate (temp_params(10))  ! Initial allocation

        ! Parse parameters separated by commas
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                ! End of parameters
                exit
            end if

            param_count = param_count + 1
            if (param_count > size(temp_params)) then
                ! Need to resize
                block
                    type(ast_node_wrapper), allocatable :: new_params(:)
                    allocate (new_params(size(temp_params)*2))
                    new_params(1:size(temp_params)) = temp_params
                    call move_alloc(new_params, temp_params)
                end block
            end if

            ! Parse a single parameter
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
          allocate (temp_params(param_count)%node, source=create_identifier(token%text))
            else if (token%kind == TK_NUMBER) then
                token = parser%consume()
                allocate(temp_params(param_count)%node, source=create_literal(token%text, LITERAL_INTEGER))
            else
                ! Unknown parameter type - consume it as identifier
                token = parser%consume()
          allocate (temp_params(param_count)%node, source=create_identifier(token%text))
            end if

            ! Check for comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! Consume comma
                cycle
            else
                exit
            end if
        end do

        ! Copy to result
        if (param_count > 0) then
            allocate (parameters(param_count))
            parameters(1:param_count) = temp_params(1:param_count)
        end if

    end subroutine parse_derived_type_parameters

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
                    allocate (wrapper_args(1))
                    allocate (wrapper_args(1)%node, source=current_arg)

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
                                allocate (new_wrapper%node, source=current_arg)
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
                allocate (print_stmt%args(0))  ! Empty wrapper array
            end if
            print_stmt%line = line
            print_stmt%column = column
            allocate (print_node, source=print_stmt)
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
            ! Error - missing function name, create empty function
            func_name = "unnamed_function"
        end if

        ! Parse parameters
        ! Look for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()

            ! Parse parameter list
            block
                type(ast_node_wrapper), allocatable :: temp_params(:)
                integer :: param_count, i

                param_count = 0

                do while (.not. parser%is_at_end())
                    token = parser%peek()

                    ! Check for closing parenthesis
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()
                        exit
                    end if

                    ! Check for comma (parameter separator)
                    if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()
                        cycle
                    end if

                    ! Parse parameter identifier
                    if (token%kind == TK_IDENTIFIER) then
                        block
                            type(ast_node_wrapper) :: param_wrapper
                            allocate(param_wrapper%node, source=create_identifier(token%text, token%line, token%column))

                            if (param_count == 0) then
                                temp_params = [param_wrapper]
                            else
                                temp_params = [temp_params, param_wrapper]
                            end if
                            param_count = param_count + 1
                        end block
                        token = parser%consume()
                    else
                        ! Skip unexpected token
                        token = parser%consume()
                    end if
                end do

                ! Copy parameters to final array
                if (param_count > 0) then
                    allocate (params(param_count))
                    do i = 1, param_count
                        allocate (params(i)%node, source=temp_params(i)%node)
                    end do
                else
                    allocate (params(0))
                end if
            end block
        else
            allocate (params(0))
        end if

        ! Parse function body (collect all statements until "end function")
        block
            type(ast_node_wrapper), allocatable :: body_statements(:)
            class(ast_node), allocatable :: stmt
            integer :: body_count, i, stmt_start
            type(token_t), allocatable :: stmt_tokens(:)

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

                ! Collect tokens for the current statement (until newline or end of line)
                stmt_start = parser%current_token
                i = stmt_start
                do while (i <= size(parser%tokens))
                    if (parser%tokens(i)%line /= token%line) exit
                    i = i + 1
                end do

                ! Extract statement tokens
                if (i > stmt_start) then
                    allocate (stmt_tokens(i - stmt_start + 1))
                    stmt_tokens(1:i - stmt_start) = parser%tokens(stmt_start:i - 1)
                    ! Add EOF token
                    stmt_tokens(i - stmt_start + 1)%kind = TK_EOF
                    stmt_tokens(i - stmt_start + 1)%text = ""
                    stmt_tokens(i - stmt_start + 1)%line = parser%tokens(i - 1)%line
                stmt_tokens(i - stmt_start + 1)%column = parser%tokens(i - 1)%column + 1

                    ! Parse the statement properly
                    stmt = parse_statement(stmt_tokens)

                    ! Only add to body if statement was successfully parsed
                    if (allocated(stmt)) then
                        ! Extend body array using wrapper pattern
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

                    ! Advance parser to next statement
                    parser%current_token = i
                else
                    ! Skip to next token if we can't parse this statement
                    token = parser%consume()
                end if
            end do

            ! Convert wrapper array to body array
            if (body_count > 0) then
                allocate (body(body_count))
                do i = 1, body_count
                    allocate (body(i)%node, source=body_statements(i)%node)
                end do
                deallocate (body_statements)
            else
                allocate (body(0))  ! Empty body
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

    ! Parse do loop: do var = start, end [, step]
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
                    stmt = parse_statement(stmt_tokens)

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

    ! Parse select case: select case (expr) ... end select
    function parse_select_case(parser) result(select_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: select_node

        type(token_t) :: select_token, case_token, lparen_token, rparen_token
        class(ast_node), allocatable :: expr
        type(case_wrapper), allocatable :: cases(:), temp_cases(:)
        type(ast_node_wrapper), allocatable :: case_body(:), temp_body(:)
        integer :: line, column, case_count, body_count
        logical :: parsing_cases

        ! Consume 'select'
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect 'case'
        case_token = parser%consume()
        if (case_token%kind /= TK_KEYWORD .or. case_token%text /= "case") then
            ! Error: expected 'case'
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '('
            return
        end if

        ! Parse expression
        expr = parse_primary(parser)

        ! Expect ')'
        rparen_token = parser%consume()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            ! Error: expected ')'
            return
        end if

        ! Parse case blocks
        case_count = 0
        parsing_cases = .true.

        do while (parsing_cases .and. parser%current_token <= size(parser%tokens))
            ! Look for case keyword or end select
            block
                type(token_t) :: current_token
                current_token = parser%peek()

                if (current_token%kind == TK_KEYWORD) then
                    if (current_token%text == "case") then
                        ! Parse a case block
                        case_token = parser%consume()

                        ! Allocate case wrapper
                        if (case_count == 0) then
                            allocate (cases(1))
                        else
                            allocate (temp_cases(case_count))
                            temp_cases = cases(1:case_count)
                            deallocate (cases)
                            allocate (cases(case_count + 1))
                            cases(1:case_count) = temp_cases
                            deallocate (temp_cases)
                        end if
                        case_count = case_count + 1

                        ! Check for default case
                        current_token = parser%peek()
        if (current_token%kind == TK_KEYWORD .and. current_token%text == "default") then
                            cases(case_count)%case_type = "case_default"
                            case_token = parser%consume()  ! consume 'default'
                        else
                            cases(case_count)%case_type = "case"
                            ! Parse case value (could be single value or range)
                            current_token = parser%peek()
             if (current_token%kind == TK_OPERATOR .and. current_token%text == "(") then
                                lparen_token = parser%consume()

                                ! Parse first value
                                block
                                   class(ast_node), allocatable :: first_val, second_val
                                    type(token_t) :: colon_token

                                    allocate (first_val, source=parse_primary(parser))

                                    ! Check for range syntax (2:5)
                                    current_token = parser%peek()
             if (current_token%kind == TK_OPERATOR .and. current_token%text == ":") then
                                        colon_token = parser%consume()
                                     allocate (second_val, source=parse_primary(parser))
                                        ! Create a binary op node to represent the range
 allocate (cases(case_count)%value, source=create_binary_op(first_val, second_val, ":"))
                                    else
                                    allocate (cases(case_count)%value, source=first_val)
                                    end if
                                end block

                                current_token = parser%peek()
             if (current_token%kind == TK_OPERATOR .and. current_token%text == ")") then
                                    rparen_token = parser%consume()
                                end if
                            end if
                        end if

                        ! Parse case body statements until next case or end
                        body_count = 0
                        do while (parser%current_token <= size(parser%tokens))
                            current_token = parser%peek()
                            if (current_token%kind == TK_KEYWORD .and. &
                   (current_token%text == "case" .or. current_token%text == "end")) exit

                            ! Parse statement tokens
                            block
                                integer :: stmt_start, stmt_end, j
                                type(token_t), allocatable :: stmt_tokens(:)
                                class(ast_node), allocatable :: stmt

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

                                    stmt = parse_statement(stmt_tokens)

                                    if (allocated(stmt)) then
                                        if (body_count == 0) then
                                            allocate (case_body(1))
                                        else
                                            allocate (temp_body(body_count))
                                            temp_body = case_body(1:body_count)
                                            deallocate (case_body)
                                            allocate (case_body(body_count + 1))
                                            case_body(1:body_count) = temp_body
                                            deallocate (temp_body)
                                        end if
                                        body_count = body_count + 1
                                      allocate (case_body(body_count)%node, source=stmt)
                                    end if

                                    deallocate (stmt_tokens)
                                end if

                                parser%current_token = stmt_end + 1
                            end block
                        end do

                        ! Store case body
                        if (body_count > 0) then
                            allocate (cases(case_count)%body(body_count))
                            cases(case_count)%body = case_body(1:body_count)
                            deallocate (case_body)
                        end if

                    else if (current_token%text == "end") then
                        ! Check for end select
                        case_token = parser%consume()  ! consume 'end'
                        current_token = parser%peek()
         if (current_token%kind == TK_KEYWORD .and. current_token%text == "select") then
                            case_token = parser%consume()  ! consume 'select'
                            parsing_cases = .false.
                        else
                            ! Not end select, error
                            parsing_cases = .false.
                        end if
                    else
                        ! Skip other tokens
                        case_token = parser%consume()
                    end if
                else
                    ! Skip non-keyword tokens
                    case_token = parser%consume()
                end if
            end block
        end do

        ! Create select case node with cases
        block
            type(select_case_node), allocatable :: sel_node
            allocate (sel_node)
            if (case_count > 0) then
      sel_node = create_select_case(expr, cases(1:case_count), line=line, column=column)
            else
                sel_node = create_select_case(expr, line=line, column=column)
            end if
            allocate (select_node, source=sel_node)
        end block

    end function parse_select_case

    ! Parse do while loop (from 'do while')
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

    ! Parse do while loop after 'do' has been consumed
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
        condition = parse_comparison(parser)

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

                    stmt = parse_statement(stmt_tokens)

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

end module parser_core
