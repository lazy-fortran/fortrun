module parser_statements_module
    ! Parser module for various statement types (use, include, print, etc.)
    use lexer_core
    use parser_state_module
    use parser_expressions_module
    use ast_core
    implicit none
    private

    public :: parse_use_statement, parse_include_statement, parse_print_statement
    public :: parse_derived_type, parse_function_definition, parse_subroutine_definition
    public :: parse_interface_block, parse_module

contains

    ! Parse use statement: use module_name [, only: item1, item2, new_name => old_name]
    function parse_use_statement(parser) result(stmt)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: stmt
        type(token_t) :: token
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: only_list(:)
        character(len=:), allocatable :: rename_list(:)
        logical :: has_only
        integer :: line, column

        ! Consume 'use' keyword
        token = parser%consume()
        line = token%line
        column = token%column

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

        has_only = .false.

        ! Check for optional only clause
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()  ! consume ','

            ! Check for 'only' keyword
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "only") then
                token = parser%consume()  ! consume 'only'
                has_only = .true.

                ! Expect ':'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ":") then
                    token = parser%consume()  ! consume ':'

                    ! Parse only list items
                    call parse_only_list(parser, only_list, rename_list)
                end if
            end if
        end if

        ! Create use statement node
stmt = create_use_statement(module_name, only_list=only_list, rename_list=rename_list, &
                                    has_only=has_only, line=line, column=column)

    end function parse_use_statement

    ! Parse include statement: include 'filename'
    function parse_include_statement(parser) result(stmt)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: stmt
        type(token_t) :: token
        character(len=:), allocatable :: filename
        integer :: line, column

        ! Consume "include" keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Expect filename (string literal)
        token = parser%consume()
        if (token%kind == TK_STRING) then
            filename = token%text
        else
            ! Error handling - for now just use empty string
            filename = ""
        end if

        ! Create include statement node
        stmt = create_include_statement(filename, line, column)

    end function parse_include_statement

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

    ! Parse only list helper
    subroutine parse_only_list(parser, only_list, rename_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: only_list(:)
        character(len=:), allocatable, intent(out) :: rename_list(:)

        character(len=100) :: temp_only(50)
        character(len=100) :: temp_rename(50)
        type(token_t) :: token
        integer :: count
        character(len=:), allocatable :: item_name, old_name

        count = 0
        temp_only = ""
        temp_rename = ""

        ! Parse first item
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            item_name = token%text

            ! Check if this is a rename (new_name => old_name)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ">") then
                    token = parser%consume()  ! consume '>'

                    ! Get old name
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                        old_name = token%text

                        ! Store rename: "new_name => old_name"
                        count = count + 1
                        temp_rename(count) = item_name//" => "//old_name
                    end if
                end if
            else
                ! Regular only item
                count = count + 1
                temp_only(count) = item_name
            end if
        end if

        ! Parse remaining items
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','

                ! Get next item
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                    item_name = token%text

                    ! Check if this is a rename
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "=") then
                        token = parser%consume()  ! consume '='
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ">") then
                            token = parser%consume()  ! consume '>'

                            ! Get old name
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER) then
                                token = parser%consume()
                                old_name = token%text

                                ! Store rename: "new_name => old_name"
                                count = count + 1
                                temp_rename(count) = item_name//" => "//old_name
                            end if
                        end if
                    else
                        ! Regular only item
                        count = count + 1
                        temp_only(count) = item_name
                    end if
                else
                    ! No more items
                    exit
                end if
            else
                ! No more items
                exit
            end if
        end do

        ! Copy to output arrays with proper size
        block
            integer :: only_count, rename_count, i

            ! Count actual items
            only_count = 0
            rename_count = 0
            do i = 1, count
                if (len_trim(temp_only(i)) > 0) only_count = only_count + 1
                if (len_trim(temp_rename(i)) > 0) rename_count = rename_count + 1
            end do

            ! Allocate output arrays
            if (only_count > 0) then
                allocate (character(len=100) :: only_list(only_count))
                only_count = 0
                do i = 1, count
                    if (len_trim(temp_only(i)) > 0) then
                        only_count = only_count + 1
                        only_list(only_count) = trim(temp_only(i))
                    end if
                end do
            end if

            if (rename_count > 0) then
                allocate (character(len=100) :: rename_list(rename_count))
                rename_count = 0
                do i = 1, count
                    if (len_trim(temp_rename(i)) > 0) then
                        rename_count = rename_count + 1
                        rename_list(rename_count) = trim(temp_rename(i))
                    end if
                end do
            end if
        end block

    end subroutine parse_only_list

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
                    stmt = parse_basic_statement(stmt_tokens)

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

    function parse_subroutine_definition(parser) result(sub_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: sub_node

        character(len=:), allocatable :: sub_name
        type(token_t) :: token
        type(ast_node_wrapper), allocatable :: params(:), body(:)
        integer :: line, column

        ! Expect "subroutine" keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "subroutine") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            ! Error: expected subroutine
            return
        end if

        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            sub_name = token%text
            token = parser%consume()
        else
            ! Error: expected subroutine name
            return
        end if

        ! Parse parameters
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('

            ! Parse parameter list inline
            block
                type(ast_node_wrapper), allocatable :: temp_params(:)
                integer :: param_count

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

                ! Allocate final parameters array
                if (param_count > 0) then
                    allocate (params(param_count))
                    params = temp_params(1:param_count)
                else
                    allocate (params(0))
                end if
            end block
        else
            ! No parameters
            allocate (params(0))
        end if

        ! Parse body (simplified for now - just consume tokens until end subroutine)
        allocate (body(0))

        ! Look for end subroutine
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "subroutine") then
                    token = parser%consume()  ! consume 'subroutine'
                    exit
                end if
            else
                token = parser%consume()  ! skip other tokens
            end if
        end do

        ! Create subroutine node
        block
            type(subroutine_def_node), allocatable :: snode
            allocate (snode)
            snode = create_subroutine_def(sub_name, params, body, line, column)
            allocate (sub_node, source=snode)
        end block

    end function parse_subroutine_definition

    function parse_interface_block(parser) result(interface_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: interface_node

        type(token_t) :: interface_token, token
        character(len=:), allocatable :: name, operator_symbol, kind
        integer :: line, column
        type(ast_node_wrapper), allocatable :: procedures(:)

        ! Consume 'interface' keyword
        interface_token = parser%consume()
        line = interface_token%line
        column = interface_token%column
        kind = "interface"

        ! Check what follows the interface keyword
        token = parser%peek()

        if (token%kind == TK_IDENTIFIER) then
            ! Named interface: interface name
            token = parser%consume()
            name = token%text
            kind = "generic"
        else if (token%kind == TK_KEYWORD .and. token%text == "operator") then
            ! Operator interface: interface operator(+)
            token = parser%consume()  ! consume 'operator'
            kind = "operator"

            ! Expect '('
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('

                ! Get operator symbol
                token = parser%peek()
                if (token%kind == TK_OPERATOR) then
                    operator_symbol = token%text
                    token = parser%consume()
                end if

                ! Expect ')'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        else if (token%kind == TK_KEYWORD .and. token%text == "assignment") then
            ! Assignment interface: interface assignment(=)
            token = parser%consume()  ! consume 'assignment'
            kind = "assignment"

            ! Expect '(=)'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()  ! consume '='
                    operator_symbol = "="
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        end if

        ! TODO: Parse interface body (procedure declarations)
        ! For now, just skip until "end interface"
        do
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "interface") then
                    token = parser%consume()  ! consume 'interface'
                    exit
                end if
            else
                token = parser%consume()  ! skip token
            end if
        end do

        ! Create interface block node
        block
            type(interface_block_node), allocatable :: node
            allocate (node)
            if (allocated(name)) then
                node = create_interface_block(name=name, kind=kind, procedures=procedures, line=line, column=column)
            else if (allocated(operator_symbol)) then
                node = create_interface_block(kind=kind, operator=operator_symbol, procedures=procedures, line=line, column=column)
            else
                node = create_interface_block(kind=kind, procedures=procedures, line=line, column=column)
            end if
            allocate (interface_node, source=node)
        end block

    end function parse_interface_block

    function parse_module(parser) result(module_ast)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: module_ast

        type(token_t) :: module_token, token
        character(len=:), allocatable :: name
        integer :: line, column
        type(ast_node_wrapper), allocatable :: declarations(:)
        type(ast_node_wrapper), allocatable :: procedures(:)
        logical :: has_contains

        ! Consume 'module' keyword
        module_token = parser%consume()
        line = module_token%line
        column = module_token%column
        has_contains = .false.

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            name = token%text
        else
            ! Error: expected module name
            name = "unknown"
        end if

        ! TODO: Parse module body (declarations)
        ! For now, just skip until "contains" or "end module"
        do
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            if (token%kind == TK_KEYWORD .and. token%text == "contains") then
                token = parser%consume()  ! consume 'contains'
                has_contains = .true.
                exit
            else if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "module") then
                    token = parser%consume()  ! consume 'module'
                    ! Optional module name after 'end module'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()  ! consume module name
                    end if
                    exit
                end if
            else
                token = parser%consume()  ! skip token
            end if
        end do

        ! TODO: Parse procedures after contains
        ! For now, just skip until "end module"
        if (has_contains) then
            do
                token = parser%peek()
                if (token%kind == TK_EOF) exit
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    token = parser%consume()  ! consume 'end'
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. token%text == "module") then
                        token = parser%consume()  ! consume 'module'
                        ! Optional module name after 'end module'
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER) then
                            token = parser%consume()  ! consume module name
                        end if
                        exit
                    end if
                else
                    token = parser%consume()  ! skip token
                end if
            end do
        end if

        ! Create module node
        block
            type(module_node), allocatable :: node
            allocate (node)
    node = create_module(name=name, has_contains=has_contains, line=line, column=column)
            allocate (module_ast, source=node)
        end block

    end function parse_module

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

end module parser_statements_module
