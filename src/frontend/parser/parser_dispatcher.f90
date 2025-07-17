module parser_dispatcher_module
    ! Statement dispatcher that delegates to appropriate parsing modules
    ! This implements the SRP by separating the switch logic from the implementations
    use lexer_core
    use parser_state_module
    use parser_expressions_module
    use parser_declarations_module
    use parser_statements_module
    use parser_control_flow_module
    use ast_core
    implicit none
    private

    public :: parse_statement_dispatcher

contains

    ! Parse a statement by dispatching to appropriate parsing module
    function parse_statement_dispatcher(tokens) result(stmt)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: stmt
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        class(ast_node), allocatable :: target, value

        parser = create_parser_state(tokens)
        first_token = parser%peek()

        ! Dispatch based on first token
        select case (first_token%kind)
        case (TK_KEYWORD)
            select case (first_token%text)
            case ("use")
                stmt = parse_use_statement(parser)
            case ("include")
                stmt = parse_include_statement(parser)
            case ("print")
                stmt = parse_print_statement(parser)
            case ("if")
                stmt = parse_if(parser)
            case ("do")
                stmt = parse_do_loop(parser)
            case ("select")
                stmt = parse_select_case(parser)
            case ("function")
                stmt = parse_function_definition(parser)
            case ("subroutine")
                stmt = parse_subroutine_definition(parser)
            case ("interface")
                stmt = parse_interface_block(parser)
            case ("module")
                stmt = parse_module(parser)
            case ("type")
                stmt = parse_type_or_declaration(parser)
            case ("real", "integer", "logical", "character", "complex")
                stmt = parse_type_or_declaration(parser)
            case default
                stmt = parse_as_expression(tokens)
            end select
        case (TK_IDENTIFIER)
            ! Could be assignment or expression
            stmt = parse_assignment_or_expression(parser)
        case default
            ! Parse as expression
            stmt = parse_as_expression(tokens)
        end select

    end function parse_statement_dispatcher

    ! Parse type declaration or derived type definition
    function parse_type_or_declaration(parser) result(stmt)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: stmt
        type(token_t) :: first_token, second_token
        logical :: is_derived_type_def

        first_token = parser%peek()
        is_derived_type_def = .false.

        if (first_token%text == "type") then
            ! Check if this is a derived type definition or variable declaration
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
            else
                stmt = parse_declaration(parser)
            end if
        else
            ! Other type keywords - check if it's a declaration
            if (has_double_colon(parser)) then
                stmt = parse_declaration(parser)
            else
                ! Could be function definition like "real function foo()"
                stmt = parse_function_or_expression(parser)
            end if
        end if

    end function parse_type_or_declaration

    ! Parse assignment or expression
    function parse_assignment_or_expression(parser) result(stmt)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: stmt
        type(token_t) :: id_token, op_token
        class(ast_node), allocatable :: target, value

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

    end function parse_assignment_or_expression

    ! Parse function definition or expression
    function parse_function_or_expression(parser) result(stmt)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: stmt
        type(token_t) :: first_token, second_token

        first_token = parser%peek()

        ! Look ahead to see if next token is "function"
        if (parser%current_token + 1 <= size(parser%tokens)) then
            second_token = parser%tokens(parser%current_token + 1)
         if (second_token%kind == TK_KEYWORD .and. second_token%text == "function") then
                stmt = parse_function_definition(parser)
                return
            end if
        end if

        ! Not a function definition, parse as expression
        stmt = parse_as_expression(parser%tokens)

    end function parse_function_or_expression

    ! Parse as expression
    function parse_as_expression(tokens) result(stmt)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: stmt

        stmt = parse_expression(tokens)
    end function parse_as_expression

    ! Helper function to check for double colon
    logical function has_double_colon(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: i

        has_double_colon = .false.
    do i = parser%current_token + 1, min(parser%current_token + 10, size(parser%tokens))
      if (parser%tokens(i)%kind == TK_OPERATOR .and. parser%tokens(i)%text == "::") then
                has_double_colon = .true.
                exit
 else if (parser%tokens(i)%kind == TK_KEYWORD .or. parser%tokens(i)%kind == TK_EOF) then
                exit
            end if
        end do
    end function has_double_colon

end module parser_dispatcher_module
