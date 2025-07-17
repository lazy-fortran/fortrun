module ast_factory
    use ast_types
    implicit none
    private

    ! Public interface for creating AST nodes
    public :: create_program, create_assignment, create_binary_op
    public :: create_function_def, create_subroutine_def, create_function_call, create_call_or_subscript
    public :: create_identifier, create_literal, create_use_statement, create_include_statement, create_print_statement
    public :: create_declaration, create_do_loop, create_do_while, create_if, create_select_case
    public :: create_derived_type, create_interface_block, create_module

contains

    function create_program(name, body, line, column) result(node)
        character(len=*), intent(in) :: name
        class(ast_node), intent(in) :: body(:)
        integer, intent(in), optional :: line, column
        type(program_node) :: node
        integer :: i

        node%name = name
        if (size(body) > 0) then
            allocate (node%body(size(body)))
            do i = 1, size(body)
                allocate (node%body(i)%node, source=body(i))
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_program

    function create_assignment(target, value, line, column, inferred_type, inferred_type_name) result(node)
        class(ast_node), intent(in) :: target
        class(ast_node), intent(in) :: value
        integer, intent(in), optional :: line, column
        logical, intent(in), optional :: inferred_type
        character(len=*), intent(in), optional :: inferred_type_name
        type(assignment_node) :: node

        allocate (node%target, source=target)
        allocate (node%value, source=value)
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        if (present(inferred_type)) node%type_was_inferred = inferred_type
        if (present(inferred_type_name)) node%inferred_type_name = inferred_type_name
    end function create_assignment

    function create_binary_op(left, right, operator, line, column) result(node)
        class(ast_node), intent(in) :: left
        class(ast_node), intent(in) :: right
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column
        type(binary_op_node) :: node

        allocate (node%left, source=left)
        allocate (node%right, source=right)
        node%operator = operator
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_binary_op

function create_function_def(name, params, return_type, body, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in) :: params(:)
        class(ast_node), intent(in) :: return_type
        type(ast_node_wrapper), intent(in) :: body(:)
        integer, intent(in), optional :: line, column
        type(function_def_node) :: node
        integer :: i

        node%name = name
        if (size(params) > 0) then
            allocate (node%params, source=params)
        end if
        allocate (node%return_type, source=return_type)
        if (size(body) > 0) then
            allocate (node%body, source=body)
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_def

    function create_subroutine_def(name, params, body, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in) :: params(:)
        type(ast_node_wrapper), intent(in) :: body(:)
        integer, intent(in), optional :: line, column
        type(subroutine_def_node) :: node
        integer :: i

        node%name = name
        if (size(params) > 0) then
            allocate (node%params, source=params)
        end if
        if (size(body) > 0) then
            allocate (node%body, source=body)
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_def

    function create_function_call(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in) :: args(:)
        integer, intent(in), optional :: line, column
        type(function_call_node) :: node
        integer :: i

        node%name = name
        if (size(args) > 0) then
            allocate (node%args(size(args)))
            do i = 1, size(args)
                allocate (node%args(i)%node, source=args(i)%node)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_call

    function create_call_or_subscript(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in) :: args(:)
        integer, intent(in), optional :: line, column
        type(call_or_subscript_node) :: node
        integer :: i

        node%name = name
        if (size(args) > 0) then
            allocate (node%args(size(args)))
            do i = 1, size(args)
                allocate (node%args(i)%node, source=args(i)%node)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_call_or_subscript

    function create_identifier(name, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column
        type(identifier_node) :: node

        node%name = name
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_identifier

    function create_literal(value, kind, line, column) result(node)
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column
        type(literal_node) :: node

        node%value = value
        node%literal_kind = kind
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_literal

    function create_declaration(type_name, var_name, kind_value, initializer, dimensions, is_allocatable, line, column) result(node)
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_name
        integer, intent(in), optional :: kind_value
        class(ast_node), allocatable, intent(in), optional :: initializer
        type(ast_node_wrapper), intent(in), optional :: dimensions(:)
        logical, intent(in), optional :: is_allocatable
        integer, intent(in), optional :: line, column
        type(declaration_node) :: node

        node%type_name = type_name
        node%var_name = var_name

        if (present(kind_value)) then
            node%kind_value = kind_value
            node%has_kind = .true.
        else
            node%kind_value = 0
            node%has_kind = .false.
        end if

        if (present(initializer)) then
            allocate (node%initializer, source=initializer)
        end if

        if (present(dimensions)) then
            node%is_array = .true.
            allocate (node%dimensions, source=dimensions)
        else
            node%is_array = .false.
        end if

        if (present(is_allocatable)) then
            node%is_allocatable = is_allocatable
        else
            node%is_allocatable = .false.
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_declaration

    function create_use_statement(module_name, only_list, rename_list, has_only, line, column) result(node)
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:)
        character(len=*), intent(in), optional :: rename_list(:)
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column
        type(use_statement_node) :: node

        node%module_name = module_name
        if (present(only_list)) then
            node%only_list = only_list
        end if
        if (present(rename_list)) then
            node%rename_list = rename_list
        end if
        if (present(has_only)) then
            node%has_only = has_only
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_use_statement

    function create_include_statement(filename, line, column) result(node)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column
        type(include_statement_node) :: node

        node%filename = filename
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_include_statement

    function create_print_statement(args, format_spec, line, column) result(node)
        type(ast_node_wrapper), intent(in) :: args(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(print_statement_node) :: node
        integer :: i

        if (size(args) > 0) then
            allocate (node%args(size(args)))
            do i = 1, size(args)
                allocate (node%args(i)%node, source=args(i)%node)
            end do
        end if
        if (present(format_spec)) node%format_spec = format_spec
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_print_statement

    function create_do_loop(var_name, start_expr, end_expr, step_expr, body, line, column) result(node)
        character(len=*), intent(in) :: var_name
        class(ast_node), intent(in) :: start_expr, end_expr
        class(ast_node), intent(in), optional :: step_expr
        class(ast_node), intent(in), optional :: body(:)
        integer, intent(in), optional :: line, column
        type(do_loop_node) :: node
        integer :: i

        node%var_name = var_name
        allocate (node%start_expr, source=start_expr)
        allocate (node%end_expr, source=end_expr)
        if (present(step_expr)) allocate (node%step_expr, source=step_expr)

        if (present(body)) then
            if (size(body) > 0) then
                allocate (node%body(size(body)))
                do i = 1, size(body)
                    allocate (node%body(i)%node, source=body(i))
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_loop

    function create_do_while(condition, body, line, column) result(node)
        class(ast_node), intent(in) :: condition
        class(ast_node), intent(in), optional :: body(:)
        integer, intent(in), optional :: line, column
        type(do_while_node) :: node
        integer :: i

        allocate (node%condition, source=condition)

        if (present(body)) then
            if (size(body) > 0) then
                allocate (node%body(size(body)))
                do i = 1, size(body)
                    allocate (node%body(i)%node, source=body(i))
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_while

    function create_if(condition, then_body, elseif_blocks, else_body, line, column) result(node)
        class(ast_node), intent(in) :: condition
        type(ast_node_wrapper), intent(in), optional :: then_body(:)
        type(elseif_wrapper), intent(in), optional :: elseif_blocks(:)
        type(ast_node_wrapper), intent(in), optional :: else_body(:)
        integer, intent(in), optional :: line, column
        type(if_node) :: node

        allocate (node%condition, source=condition)

        if (present(then_body) .and. size(then_body) > 0) then
            allocate (node%then_body, source=then_body)
        end if

        if (present(elseif_blocks) .and. size(elseif_blocks) > 0) then
            allocate (node%elseif_blocks, source=elseif_blocks)
        end if

        if (present(else_body) .and. size(else_body) > 0) then
            allocate (node%else_body, source=else_body)
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_if

    function create_select_case(expr, cases, line, column) result(node)
        class(ast_node), intent(in) :: expr
        type(case_wrapper), intent(in), optional :: cases(:)
        integer, intent(in), optional :: line, column
        type(select_case_node) :: node
        integer :: i

        allocate (node%expr, source=expr)

        if (present(cases) .and. size(cases) > 0) then
            allocate (node%cases(size(cases)))
            node%cases = cases
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_select_case

   function create_derived_type(name, components, parameters, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in), optional :: components(:)
        type(ast_node_wrapper), intent(in), optional :: parameters(:)
        integer, intent(in), optional :: line, column
        type(derived_type_node) :: node

        node%name = name

        if (present(components)) then
            if (size(components) > 0) then
                allocate (node%components, source=components)
            end if
        end if

        if (present(parameters)) then
            if (size(parameters) > 0) then
                node%has_parameters = .true.
                allocate (node%parameters, source=parameters)
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_derived_type

    function create_interface_block(name, kind, operator, procedures, line, column) result(node)
        character(len=*), intent(in), optional :: name
        character(len=*), intent(in) :: kind
        character(len=*), intent(in), optional :: operator
        type(ast_node_wrapper), intent(in), optional :: procedures(:)
        integer, intent(in), optional :: line, column
        type(interface_block_node) :: node

        if (present(name)) then
            node%name = name
        end if

        node%kind = kind

        if (present(operator)) then
            node%operator = operator
        end if

        if (present(procedures)) then
            if (size(procedures) > 0) then
                allocate (node%procedures, source=procedures)
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_interface_block

    function create_module(name, declarations, procedures, has_contains, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in), optional :: declarations(:)
        type(ast_node_wrapper), intent(in), optional :: procedures(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(module_node) :: node

        node%name = name

        if (present(declarations)) then
            allocate (node%declarations(size(declarations)))
            node%declarations = declarations
        end if

        if (present(procedures)) then
            allocate (node%procedures(size(procedures)))
            node%procedures = procedures
        end if

        if (present(has_contains)) then
            node%has_contains = has_contains
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_module

end module ast_factory
