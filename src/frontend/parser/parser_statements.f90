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

    ! Add placeholders for other functions to be moved from parser_core
    function parse_derived_type(parser) result(type_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: type_node
        ! Implementation to be moved from parser_core
        allocate (type_node)
    end function parse_derived_type

    function parse_function_definition(parser) result(func_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: func_node
        ! Implementation to be moved from parser_core
        allocate (func_node)
    end function parse_function_definition

    function parse_subroutine_definition(parser) result(sub_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: sub_node
        ! Implementation to be moved from parser_core
        allocate (sub_node)
    end function parse_subroutine_definition

    function parse_interface_block(parser) result(interface_node)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: interface_node
        ! Implementation to be moved from parser_core
        allocate (interface_node)
    end function parse_interface_block

    function parse_module(parser) result(module_ast)
        type(parser_state_t), intent(inout) :: parser
        class(ast_node), allocatable :: module_ast
        ! Implementation to be moved from parser_core
        allocate (module_ast)
    end function parse_module

end module parser_statements_module
