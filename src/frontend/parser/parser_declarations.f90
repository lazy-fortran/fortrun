module parser_declarations_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_NUMBER, TK_STRING, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD
    use ast_core
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_comparison
    implicit none
    private

    ! Public declaration parsing interface
    public :: parse_declaration, parse_derived_type

contains

    ! Parse a variable declaration
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
                node = create_declaration(type_name, var_name, &
                                is_allocatable=is_allocatable, line=line, column=column)
            end if
            allocate (decl_node, source=node)
        end block

    end function parse_declaration

    ! Parse array dimensions helper
    subroutine parse_array_dimensions(parser, dimensions)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_wrapper), allocatable, intent(out) :: dimensions(:)

        type(token_t) :: token
        type(ast_node_wrapper), allocatable :: temp_dims(:)
        integer :: dim_count
        character(len=:), allocatable :: dim_spec
        class(ast_node), allocatable :: lower_bound, upper_bound
        integer :: line, column

        dim_count = 0
        allocate (dimensions(0))

        do
            token = parser%peek()
            line = token%line
            column = token%column

            ! Check for : (assumed shape)
            if (token%kind == TK_OPERATOR .and. token%text == ":") then
                token = parser%consume()

                ! Create assumed shape dimension node
                block
                    type(ast_node_wrapper) :: new_dim
       allocate (new_dim%node, source=create_literal(":", LITERAL_STRING, line, column))
                    dimensions = [dimensions, new_dim]
                    dim_count = dim_count + 1
                end block

            else
                ! Parse lower bound expression
                lower_bound = parse_comparison(parser)

                if (allocated(lower_bound)) then
                    token = parser%peek()

                    ! Check for : (range)
                    if (token%kind == TK_OPERATOR .and. token%text == ":") then
                        token = parser%consume()

                        ! Parse upper bound
                        upper_bound = parse_comparison(parser)

                        if (allocated(upper_bound)) then
                            ! Create range dimension (lower:upper)
                            block
                                type(ast_node_wrapper) :: new_dim
                                type(binary_op_node), allocatable :: range_node
                                allocate (range_node)
              range_node = create_binary_op(lower_bound, upper_bound, ":", line, column)
                                allocate (new_dim%node, source=range_node)
                                dimensions = [dimensions, new_dim]
                                dim_count = dim_count + 1
                            end block
                        end if
                    else
                        ! Single dimension (just the size)
                        block
                            type(ast_node_wrapper) :: new_dim
                            allocate (new_dim%node, source=lower_bound)
                            dimensions = [dimensions, new_dim]
                            dim_count = dim_count + 1
                        end block
                    end if
                end if
            end if

            ! Check for comma (more dimensions)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else
                exit
            end if
        end do

    end subroutine parse_array_dimensions

    ! Parse derived type definition: type :: type_name or type(params) :: type_name
    function parse_derived_type(parser) result(type_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: type_node

        type(token_t) :: token
        character(len=:), allocatable :: type_name
        type(ast_node_wrapper), allocatable :: components(:), params(:)
        integer :: line, column

        ! Already consumed 'type' keyword
        token = parser%peek()
        line = token%line
        column = token%column

        ! Check for optional :: or (
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            ! Simple form: type :: name
            token = parser%consume()
        else if (token%kind == TK_OPERATOR .and. token%text == "(") then
            ! Parameterized form: type(params) :: name
            token = parser%consume()  ! consume '('

            ! Parse parameters
            call parse_derived_type_parameters(parser, params)

            ! Consume ')'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if

            ! Consume '::'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "::") then
                token = parser%consume()
            end if
        else if (token%kind == TK_IDENTIFIER) then
            ! Alternative syntax without :: (type point)
            ! Do nothing, just proceed to get the type name
        end if

        ! Get type name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            type_name = token%text
        else
            ! Error: expected type name
   type_node = create_literal("ERROR: Expected type name", LITERAL_STRING, line, column)
            return
        end if

        ! TODO: Parse type components until 'end type'
        ! For now, just create the type node
        if (allocated(params)) then
            type_node = create_derived_type(type_name, components, params, line, column)
        else
            allocate (components(0))
        type_node = create_derived_type(type_name, components, line=line, column=column)
        end if

    end function parse_derived_type

    ! Parse derived type parameters
    subroutine parse_derived_type_parameters(parser, params)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_wrapper), allocatable, intent(out) :: params(:)

        type(token_t) :: token
        integer :: param_count

        param_count = 0
        allocate (params(0))

        do
            token = parser%peek()

            if (token%kind == TK_IDENTIFIER) then
                ! Parse parameter name
                token = parser%consume()

                ! Create parameter node
                block
                    type(ast_node_wrapper) :: new_param
                    allocate (new_param%node, source=create_identifier(token%text, token%line, token%column))
                    params = [params, new_param]
                    param_count = param_count + 1
                end block

                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                else
                    exit
                end if
            else
                exit
            end if
        end do

    end subroutine parse_derived_type_parameters

end module parser_declarations_module
