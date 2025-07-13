module ast_core
    use json_module
    ! use type_system_hm, only: mono_type_t  ! TODO: integrate HM type system
    implicit none
    private

    ! Base AST node type used by all dialects
    type, abstract, public :: ast_node
        integer :: line = 1
        integer :: column = 1
        ! type(mono_type_t), allocatable :: inferred_type  ! TODO: Type information from semantic analysis
    contains
        procedure(visit_interface), deferred :: accept
        procedure(to_json_interface), deferred :: to_json
    end type ast_node

    ! Abstract interfaces for visitor pattern and JSON serialization
    abstract interface
        subroutine visit_interface(this, visitor)
            import :: ast_node
            class(ast_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine visit_interface

        subroutine to_json_interface(this, json, parent)
            use json_module
            import :: ast_node
            class(ast_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine to_json_interface
    end interface

    ! Core AST node types shared by all Fortran dialects

    ! Program node
    type, extends(ast_node), public :: program_node
        character(len=:), allocatable :: name
        class(ast_node), allocatable :: body(:)
    contains
        procedure :: accept => program_accept
        procedure :: to_json => program_to_json
    end type program_node

    ! Assignment node
    type, extends(ast_node), public :: assignment_node
        class(ast_node), allocatable :: target
        class(ast_node), allocatable :: value
    contains
        procedure :: accept => assignment_accept
        procedure :: to_json => assignment_to_json
    end type assignment_node

    ! Binary operation node
    type, extends(ast_node), public :: binary_op_node
        class(ast_node), allocatable :: left
        class(ast_node), allocatable :: right
        character(len=:), allocatable :: operator
    contains
        procedure :: accept => binary_op_accept
        procedure :: to_json => binary_op_to_json
    end type binary_op_node

    ! Function definition node
    type, extends(ast_node), public :: function_def_node
        character(len=:), allocatable :: name
        class(ast_node), allocatable :: params(:)
        class(ast_node), allocatable :: return_type
        class(ast_node), allocatable :: body(:)
    contains
        procedure :: accept => function_def_accept
        procedure :: to_json => function_def_to_json
    end type function_def_node

    ! Subroutine definition node
    type, extends(ast_node), public :: subroutine_def_node
        character(len=:), allocatable :: name
        class(ast_node), allocatable :: params(:)
        class(ast_node), allocatable :: body(:)
    contains
        procedure :: accept => subroutine_def_accept
        procedure :: to_json => subroutine_def_to_json
    end type subroutine_def_node

    ! Function call node
    type, extends(ast_node), public :: function_call_node
        character(len=:), allocatable :: name
        class(ast_node), allocatable :: args(:)
    contains
        procedure :: accept => function_call_accept
        procedure :: to_json => function_call_to_json
    end type function_call_node

    ! Identifier node
    type, extends(ast_node), public :: identifier_node
        character(len=:), allocatable :: name
    contains
        procedure :: accept => identifier_accept
        procedure :: to_json => identifier_to_json
    end type identifier_node

    ! Literal node
    type, extends(ast_node), public :: literal_node
        character(len=:), allocatable :: value
        integer :: literal_kind = 0  ! INTEGER_LITERAL, REAL_LITERAL, etc.
    contains
        procedure :: accept => literal_accept
        procedure :: to_json => literal_to_json
    end type literal_node

    ! Use statement node
    type, extends(ast_node), public :: use_statement_node
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: only_list(:)  ! Optional only clause
    contains
        procedure :: accept => use_statement_accept
        procedure :: to_json => use_statement_to_json
    end type use_statement_node

    ! Print statement node
    type, extends(ast_node), public :: print_statement_node
        character(len=:), allocatable :: format_spec  ! Optional format
        class(ast_node), allocatable :: args(:)
    contains
        procedure :: accept => print_statement_accept
        procedure :: to_json => print_statement_to_json
    end type print_statement_node

    ! Literal kind constants
    integer, parameter, public :: LITERAL_INTEGER = 1
    integer, parameter, public :: LITERAL_REAL = 2
    integer, parameter, public :: LITERAL_STRING = 3
    integer, parameter, public :: LITERAL_LOGICAL = 4

    ! Public interface for creating nodes
    public :: create_program, create_assignment, create_binary_op
    public :: create_function_def, create_subroutine_def, create_function_call
    public :: create_identifier, create_literal, create_use_statement, create_print_statement

contains

    ! Factory functions for creating AST nodes

    function create_program(name, body, line, column) result(node)
        character(len=*), intent(in) :: name
        class(ast_node), intent(in) :: body(:)
        integer, intent(in), optional :: line, column
        type(program_node) :: node
        integer :: i
        
        node%name = name
        if (size(body) > 0) then
            allocate(node%body, source=body)
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_program

    function create_assignment(target, value, line, column) result(node)
        class(ast_node), intent(in) :: target
        class(ast_node), intent(in) :: value
        integer, intent(in), optional :: line, column
        type(assignment_node) :: node
        
        allocate(node%target, source=target)
        allocate(node%value, source=value)
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_assignment

    function create_binary_op(left, right, operator, line, column) result(node)
        class(ast_node), intent(in) :: left
        class(ast_node), intent(in) :: right
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column
        type(binary_op_node) :: node
        
        allocate(node%left, source=left)
        allocate(node%right, source=right)
        node%operator = operator
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_binary_op

    function create_function_def(name, params, return_type, body, line, column) result(node)
        character(len=*), intent(in) :: name
        class(ast_node), intent(in) :: params(:)
        class(ast_node), intent(in) :: return_type
        class(ast_node), intent(in) :: body(:)
        integer, intent(in), optional :: line, column
        type(function_def_node) :: node
        integer :: i
        
        node%name = name
        if (size(params) > 0) then
            allocate(node%params, source=params)
        end if
        allocate(node%return_type, source=return_type)
        if (size(body) > 0) then
            allocate(node%body, source=body)
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_def

    function create_subroutine_def(name, params, body, line, column) result(node)
        character(len=*), intent(in) :: name
        class(ast_node), intent(in) :: params(:)
        class(ast_node), intent(in) :: body(:)
        integer, intent(in), optional :: line, column
        type(subroutine_def_node) :: node
        integer :: i
        
        node%name = name
        if (size(params) > 0) then
            allocate(node%params, source=params)
        end if
        if (size(body) > 0) then
            allocate(node%body, source=body)
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_def

    function create_function_call(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        class(ast_node), intent(in) :: args(:)
        integer, intent(in), optional :: line, column
        type(function_call_node) :: node
        integer :: i
        
        node%name = name
        if (size(args) > 0) then
            allocate(node%args, source=args)
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_call

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

    function create_use_statement(module_name, only_list, line, column) result(node)
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:)
        integer, intent(in), optional :: line, column
        type(use_statement_node) :: node
        
        node%module_name = module_name
        if (present(only_list)) then
            node%only_list = only_list
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_use_statement

    function create_print_statement(args, format_spec, line, column) result(node)
        class(ast_node), intent(in) :: args(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(print_statement_node) :: node
        integer :: i
        
        if (size(args) > 0) then
            allocate(node%args, source=args)
        end if
        if (present(format_spec)) node%format_spec = format_spec
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_print_statement

    ! Visitor pattern implementations (placeholder for now)

    subroutine program_accept(this, visitor)
        class(program_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine program_accept

    subroutine assignment_accept(this, visitor)
        class(assignment_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine assignment_accept

    subroutine binary_op_accept(this, visitor)
        class(binary_op_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine binary_op_accept

    subroutine function_def_accept(this, visitor)
        class(function_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine function_def_accept

    subroutine subroutine_def_accept(this, visitor)
        class(subroutine_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine subroutine_def_accept

    subroutine function_call_accept(this, visitor)
        class(function_call_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine function_call_accept

    subroutine identifier_accept(this, visitor)
        class(identifier_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine identifier_accept

    subroutine literal_accept(this, visitor)
        class(literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine literal_accept

    subroutine use_statement_accept(this, visitor)
        class(use_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine use_statement_accept

    subroutine print_statement_accept(this, visitor)
        class(print_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implementation depends on specific visitor
    end subroutine print_statement_accept

    ! JSON serialization implementations

    subroutine program_to_json(this, json, parent)
        class(program_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, body_array
        integer :: i
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'program')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)
        
        do i = 1, size(this%body)
            call this%body(i)%to_json(json, body_array)
        end do
        
        call json%add(parent, obj)
    end subroutine program_to_json

    subroutine assignment_to_json(this, json, parent)
        class(assignment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'assignment')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        block
            type(json_value), pointer :: target_obj, value_obj
            call json%create_object(target_obj, 'target')
            call this%target%to_json(json, target_obj)
            call json%add(obj, target_obj)
            
            call json%create_object(value_obj, 'value')
            call this%value%to_json(json, value_obj)
            call json%add(obj, value_obj)
        end block
        
        call json%add(parent, obj)
    end subroutine assignment_to_json

    subroutine binary_op_to_json(this, json, parent)
        class(binary_op_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'binary_op')
        call json%add(obj, 'operator', this%operator)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        block
            type(json_value), pointer :: left_obj, right_obj
            call json%create_object(left_obj, 'left')
            call this%left%to_json(json, left_obj)
            call json%add(obj, left_obj)
            
            call json%create_object(right_obj, 'right')
            call this%right%to_json(json, right_obj)
            call json%add(obj, right_obj)
        end block
        
        call json%add(parent, obj)
    end subroutine binary_op_to_json

    subroutine function_def_to_json(this, json, parent)
        class(function_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'function_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        call json%create_array(params_array, 'params')
        call json%add(obj, params_array)
        do i = 1, size(this%params)
            call this%params(i)%to_json(json, params_array)
        end do
        
        block
            type(json_value), pointer :: return_type_obj
            call json%create_object(return_type_obj, 'return_type')
            call this%return_type%to_json(json, return_type_obj)
            call json%add(obj, return_type_obj)
        end block
        
        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)
        do i = 1, size(this%body)
            call this%body(i)%to_json(json, body_array)
        end do
        
        call json%add(parent, obj)
    end subroutine function_def_to_json

    subroutine subroutine_def_to_json(this, json, parent)
        class(subroutine_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'subroutine_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        call json%create_array(params_array, 'params')
        call json%add(obj, params_array)
        do i = 1, size(this%params)
            call this%params(i)%to_json(json, params_array)
        end do
        
        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)
        do i = 1, size(this%body)
            call this%body(i)%to_json(json, body_array)
        end do
        
        call json%add(parent, obj)
    end subroutine subroutine_def_to_json

    subroutine function_call_to_json(this, json, parent)
        class(function_call_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'function_call')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        do i = 1, size(this%args)
            call this%args(i)%to_json(json, args_array)
        end do
        
        call json%add(parent, obj)
    end subroutine function_call_to_json

    subroutine identifier_to_json(this, json, parent)
        class(identifier_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'identifier')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        call json%add(parent, obj)
    end subroutine identifier_to_json

    subroutine literal_to_json(this, json, parent)
        class(literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        character(len=:), allocatable :: kind_name
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'literal')
        call json%add(obj, 'value', this%value)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        select case (this%literal_kind)
        case (LITERAL_INTEGER)
            kind_name = 'integer'
        case (LITERAL_REAL)
            kind_name = 'real'
        case (LITERAL_STRING)
            kind_name = 'string'
        case (LITERAL_LOGICAL)
            kind_name = 'logical'
        case default
            kind_name = 'unknown'
        end select
        call json%add(obj, 'kind', kind_name)
        
        call json%add(parent, obj)
    end subroutine literal_to_json

    subroutine use_statement_to_json(this, json, parent)
        class(use_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, only_array
        integer :: i
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'use_statement')
        call json%add(obj, 'module_name', this%module_name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        if (allocated(this%only_list)) then
            call json%create_array(only_array, 'only_list')
            call json%add(obj, only_array)
            do i = 1, size(this%only_list)
                call json%add(only_array, '', this%only_list(i))
            end do
        end if
        
        call json%add(parent, obj)
    end subroutine use_statement_to_json

    subroutine print_statement_to_json(this, json, parent)
        class(print_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'print_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        
        if (allocated(this%format_spec)) then
            call json%add(obj, 'format_spec', this%format_spec)
        end if
        
        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        do i = 1, size(this%args)
            call this%args(i)%to_json(json, args_array)
        end do
        
        call json%add(parent, obj)
    end subroutine print_statement_to_json

end module ast_core