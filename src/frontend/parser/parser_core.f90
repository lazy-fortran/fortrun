module parser_core
    use lexer_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression, parse_logical_or, parse_logical_and, &
          parse_comparison, parse_member_access, parse_term, parse_factor, parse_primary
    use parser_statements_module, only: parse_function_definition, parse_subroutine_definition, &
                                        parse_interface_block, parse_module
    use parser_control_flow_module, only: parse_do_loop, parse_do_while, parse_select_case, parse_if, &
                                          parse_if_condition, parse_if_body
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use ast_core, only: ast_node, ast_node_wrapper, assignment_node, binary_op_node, &
             identifier_node, literal_node, call_or_subscript_node, function_def_node, &
                        subroutine_def_node, print_statement_node, use_statement_node, &
                include_statement_node, declaration_node, do_loop_node, do_while_node, &
                        if_node, elseif_wrapper, select_case_node, case_wrapper, &
                        derived_type_node, interface_block_node, module_node, &
                        create_assignment, create_binary_op, create_identifier, &
                        create_literal, create_call_or_subscript, create_function_def, &
                        create_subroutine_def, create_print_statement, &
                   create_use_statement, create_include_statement, create_declaration, &
                       create_do_loop, create_do_while, create_if, create_select_case, &
                        create_derived_type, create_interface_block, create_module, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    implicit none
    private

    ! Public parsing interface
    public :: parse_expression, parse_statement
    public :: parse_primary
    public :: parse_function_definition, parse_subroutine_definition

contains

    ! Parse a statement by delegating to the dispatcher
    function parse_statement(tokens) result(stmt)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: stmt

        ! NOTE: This function is temporarily disabled during arena conversion
        ! Update to use arena-based API when available
        stmt = create_literal("! Statement parsing disabled during arena conversion", LITERAL_STRING, 1, 1)

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
                initializer = create_literal("! Parsing disabled", LITERAL_STRING, 1, 1)

                ! Store the initializer in the declaration node (will be handled in create_declaration)
                if (allocated(initializer)) then
                    ! Create declaration node with initializer
                    block
                        type(declaration_node), allocatable :: node
                        allocate (node)
                        if (has_kind .and. is_array) then
                            node = create_declaration(type_name, var_name, &
                                       kind_value=kind_value, initializer=initializer, &
                                 dimensions=dimensions, is_allocatable=is_allocatable, &
                                                      line=line, column=column)
                        else if (has_kind) then
                            node = create_declaration(type_name, var_name, &
                                       kind_value=kind_value, initializer=initializer, &
                                is_allocatable=is_allocatable, line=line, column=column)
                        else if (is_array) then
                            node = create_declaration(type_name, var_name, &
                                       initializer=initializer, dimensions=dimensions, &
                                is_allocatable=is_allocatable, line=line, column=column)
                        else
                            node = create_declaration(type_name, var_name, &
                               initializer=initializer, is_allocatable=is_allocatable, &
                                                      line=line, column=column)
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
                node = create_declaration(type_name, var_name, &
                                         kind_value=kind_value, dimensions=dimensions, &
                                is_allocatable=is_allocatable, line=line, column=column)
            else if (has_kind) then
                node = create_declaration(type_name, var_name, &
                                 kind_value=kind_value, is_allocatable=is_allocatable, &
                                          line=line, column=column)
            else if (is_array) then
                node = create_declaration(type_name, var_name, &
                                 dimensions=dimensions, is_allocatable=is_allocatable, &
                                          line=line, column=column)
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
                            allocate (temp_dims(dim_count)%node, &
                   source=create_literal(first_number//":"//token%text, LITERAL_STRING))
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
                current_arg = create_literal("! Parsing disabled", LITERAL_STRING, 1, 1)
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
                current_arg = create_literal("! Parsing disabled", LITERAL_STRING, 1, 1)
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

        ! Create print statement node with arena-based args
        block
            type(print_statement_node) :: print_stmt
            print_stmt%format_spec = format_spec
            ! Convert wrapper_args to arena indices when arena is available
            allocate (print_stmt%arg_indices(0))  ! Empty indices array for now
            print_stmt%line = line
            print_stmt%column = column
            allocate (print_node, source=print_stmt)
        end block

    end function parse_print_statement

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

    ! Parse do loop: do var = start, end [, step]

    ! Parse interface block: interface [name|operator(op)] ... end interface

    ! Parse only list: item1, item2, new_name => old_name
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

    ! Parse if statement

    ! Parse elseif block
    function parse_elseif_block(parser) result(elseif_block)
        type(parser_state_t), intent(inout) :: parser
        type(elseif_wrapper) :: elseif_block
        type(token_t) :: elseif_token

        ! Consume 'elseif' or 'else if'
        elseif_token = parser%consume()

        ! NOTE: This function is temporarily disabled during arena conversion
        ! Update to use arena-based API when available
        elseif_block%condition_index = 0  ! Disabled during arena conversion

        ! Look for 'then' keyword
        elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. elseif_token%text == "then") then
            elseif_token = parser%consume()
        end if

        ! NOTE: This function is temporarily disabled during arena conversion
        ! Update to use arena-based API when available
        allocate (elseif_block%body_indices(0))  ! Empty indices array

    end function parse_elseif_block

end module parser_core
