module ast_types
    use json_module
    use type_system_hm, only: mono_type_t
    implicit none
    private

    ! Base AST node type used by all dialects
    type, abstract, public :: ast_node
        integer :: line = 1
        integer :: column = 1
        type(mono_type_t), allocatable :: inferred_type  ! Type information from semantic analysis
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
        type(ast_node_wrapper), allocatable :: body(:)
    contains
        procedure :: accept => program_accept
        procedure :: to_json => program_to_json
    end type program_node

    ! Assignment node
    type, extends(ast_node), public :: assignment_node
        class(ast_node), allocatable :: target
        class(ast_node), allocatable :: value
        ! Type inference support (dialect-agnostic)
        logical :: type_was_inferred = .false.  ! true if type was inferred
        character(len=:), allocatable :: inferred_type_name
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
        type(ast_node_wrapper), allocatable :: params(:)
        class(ast_node), allocatable :: return_type
        type(ast_node_wrapper), allocatable :: body(:)
    contains
        procedure :: accept => function_def_accept
        procedure :: to_json => function_def_to_json
    end type function_def_node

    ! Subroutine definition node
    type, extends(ast_node), public :: subroutine_def_node
        character(len=:), allocatable :: name
        type(ast_node_wrapper), allocatable :: params(:)
        type(ast_node_wrapper), allocatable :: body(:)
    contains
        procedure :: accept => subroutine_def_accept
        procedure :: to_json => subroutine_def_to_json
    end type subroutine_def_node

    ! Function call node
    type, extends(ast_node), public :: function_call_node
        character(len=:), allocatable :: name
        type(ast_node_wrapper), allocatable :: args(:)
    contains
        procedure :: accept => function_call_accept
        procedure :: to_json => function_call_to_json
    end type function_call_node

    ! Call or subscript node (represents both function calls and array indexing)
    ! In Fortran, both use the same syntax: name(args)
    type, extends(ast_node), public :: call_or_subscript_node
        character(len=:), allocatable :: name
        type(ast_node_wrapper), allocatable :: args(:)
    contains
        procedure :: accept => call_or_subscript_accept
        procedure :: to_json => call_or_subscript_to_json
    end type call_or_subscript_node

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
        character(len=:), allocatable :: only_list(:)     ! Optional only clause items
        character(len=:), allocatable :: rename_list(:)   ! Optional rename mappings (new_name => old_name)
        logical :: has_only = .false.                     ! Whether the only clause is present
    contains
        procedure :: accept => use_statement_accept
        procedure :: to_json => use_statement_to_json
    end type use_statement_node

    ! Include statement node
    type, extends(ast_node), public :: include_statement_node
        character(len=:), allocatable :: filename
    contains
        procedure :: accept => include_statement_accept
        procedure :: to_json => include_statement_to_json
    end type include_statement_node

    ! Print statement node
    type, extends(ast_node), public :: print_statement_node
        character(len=:), allocatable :: format_spec  ! Optional format
        type(ast_node_wrapper), allocatable :: args(:)
    contains
        procedure :: accept => print_statement_accept
        procedure :: to_json => print_statement_to_json
    end type print_statement_node

    ! Declaration node
    type, extends(ast_node), public :: declaration_node
        character(len=:), allocatable :: type_name     ! real, integer, etc.
        character(len=:), allocatable :: var_name      ! Variable name
        integer :: kind_value                          ! Kind parameter (e.g., 8 for real(8))
        logical :: has_kind                            ! Whether kind was specified
        class(ast_node), allocatable :: initializer    ! Optional initialization value
        ! Array dimension support
        logical :: is_array = .false.                  ! Whether this is an array declaration
        type(ast_node_wrapper), allocatable :: dimensions(:) ! Array dimensions
        logical :: is_allocatable = .false.           ! Whether allocatable attribute is present
    contains
        procedure :: accept => declaration_accept
        procedure :: to_json => declaration_to_json
    end type declaration_node

    ! Do loop node
    type, extends(ast_node), public :: do_loop_node
        character(len=:), allocatable :: var_name     ! Loop variable
        class(ast_node), allocatable :: start_expr    ! Start expression
        class(ast_node), allocatable :: end_expr      ! End expression
        class(ast_node), allocatable :: step_expr     ! Step expression (optional)
        type(ast_node_wrapper), allocatable :: body(:) ! Loop body
    contains
        procedure :: accept => do_loop_accept
        procedure :: to_json => do_loop_to_json
    end type do_loop_node

    ! Do while loop node
    type, extends(ast_node), public :: do_while_node
        class(ast_node), allocatable :: condition     ! While condition
        type(ast_node_wrapper), allocatable :: body(:) ! Loop body
    contains
        procedure :: accept => do_while_accept
        procedure :: to_json => do_while_to_json
    end type do_while_node

    ! If statement node
    type, extends(ast_node), public :: if_node
        class(ast_node), allocatable :: condition      ! If condition
        type(ast_node_wrapper), allocatable :: then_body(:)  ! Then body
        type(elseif_wrapper), allocatable :: elseif_blocks(:) ! Elseif blocks (optional)
        type(ast_node_wrapper), allocatable :: else_body(:)  ! Else body (optional)
    contains
        procedure :: accept => if_accept
        procedure :: to_json => if_to_json
    end type if_node

    ! Elseif wrapper (not an AST node itself)
    type, public :: elseif_wrapper
        class(ast_node), allocatable :: condition     ! Elseif condition
        type(ast_node_wrapper), allocatable :: body(:) ! Elseif body
    end type elseif_wrapper

    ! Derived type definition node
    type, extends(ast_node), public :: derived_type_node
        character(len=:), allocatable :: name          ! Type name
        type(ast_node_wrapper), allocatable :: components(:) ! Type components
        logical :: has_parameters = .false.            ! Whether it has parameters
        type(ast_node_wrapper), allocatable :: parameters(:) ! Type parameters
    contains
        procedure :: accept => derived_type_accept
        procedure :: to_json => derived_type_to_json
    end type derived_type_node

    ! Select case node
    type, extends(ast_node), public :: select_case_node
        class(ast_node), allocatable :: expr          ! Expression to match
        type(case_wrapper), allocatable :: cases(:)   ! Case statements
    contains
        procedure :: accept => select_case_accept
        procedure :: to_json => select_case_to_json
    end type select_case_node

    ! Interface block node
    type, extends(ast_node), public :: interface_block_node
        character(len=:), allocatable :: name         ! Interface name (optional)
        character(len=:), allocatable :: kind         ! "interface", "generic", "operator", "assignment"
        character(len=:), allocatable :: operator     ! Operator symbol (for operator interfaces)
        type(ast_node_wrapper), allocatable :: procedures(:) ! Procedure declarations
    contains
        procedure :: accept => interface_block_accept
        procedure :: to_json => interface_block_to_json
    end type interface_block_node

    ! Module node
    type, extends(ast_node), public :: module_node
        character(len=:), allocatable :: name         ! Module name
        type(ast_node_wrapper), allocatable :: declarations(:) ! Module declarations
        type(ast_node_wrapper), allocatable :: procedures(:)   ! Module procedures (after contains)
        logical :: has_contains = .false.             ! Whether module has a contains section
    contains
        procedure :: accept => module_accept
        procedure :: to_json => module_to_json
    end type module_node

    ! Case statement wrapper
    type, public :: case_wrapper
        character(len=:), allocatable :: case_type    ! "case", "case_default"
        class(ast_node), allocatable :: value         ! Case value (optional for default)
        type(ast_node_wrapper), allocatable :: body(:) ! Case body
    end type case_wrapper

    ! Wrapper type for polymorphic arrays - concrete type containing abstract member
    type, public :: ast_node_wrapper
        class(ast_node), allocatable :: node
    end type ast_node_wrapper

    ! Literal kind constants
    integer, parameter, public :: LITERAL_INTEGER = 1
    integer, parameter, public :: LITERAL_REAL = 2
    integer, parameter, public :: LITERAL_STRING = 3
    integer, parameter, public :: LITERAL_LOGICAL = 4

    ! Forward declarations for procedures (will be implemented in other modules)
    interface
        subroutine program_accept(this, visitor)
            import :: ast_node
            class(program_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine program_accept

        subroutine assignment_accept(this, visitor)
            import :: ast_node
            class(assignment_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine assignment_accept

        subroutine binary_op_accept(this, visitor)
            import :: ast_node
            class(binary_op_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine binary_op_accept

        subroutine function_def_accept(this, visitor)
            import :: ast_node
            class(function_def_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine function_def_accept

        subroutine subroutine_def_accept(this, visitor)
            import :: ast_node
            class(subroutine_def_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine subroutine_def_accept

        subroutine function_call_accept(this, visitor)
            import :: ast_node
            class(function_call_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine function_call_accept

        subroutine call_or_subscript_accept(this, visitor)
            import :: ast_node
            class(call_or_subscript_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine call_or_subscript_accept

        subroutine identifier_accept(this, visitor)
            import :: ast_node
            class(identifier_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine identifier_accept

        subroutine literal_accept(this, visitor)
            import :: ast_node
            class(literal_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine literal_accept

        subroutine use_statement_accept(this, visitor)
            import :: ast_node
            class(use_statement_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine use_statement_accept

        subroutine include_statement_accept(this, visitor)
            import :: ast_node
            class(include_statement_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine include_statement_accept

        subroutine print_statement_accept(this, visitor)
            import :: ast_node
            class(print_statement_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine print_statement_accept

        subroutine declaration_accept(this, visitor)
            import :: ast_node
            class(declaration_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine declaration_accept

        subroutine do_loop_accept(this, visitor)
            import :: ast_node
            class(do_loop_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine do_loop_accept

        subroutine do_while_accept(this, visitor)
            import :: ast_node
            class(do_while_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine do_while_accept

        subroutine if_accept(this, visitor)
            import :: ast_node
            class(if_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine if_accept

        subroutine select_case_accept(this, visitor)
            import :: ast_node
            class(select_case_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine select_case_accept

        subroutine derived_type_accept(this, visitor)
            import :: ast_node
            class(derived_type_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine derived_type_accept

        subroutine interface_block_accept(this, visitor)
            import :: ast_node
            class(interface_block_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine interface_block_accept

        subroutine module_accept(this, visitor)
            import :: ast_node
            class(module_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine module_accept

        ! JSON serialization interfaces
        subroutine program_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(program_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine program_to_json

        subroutine assignment_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(assignment_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine assignment_to_json

        subroutine binary_op_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(binary_op_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine binary_op_to_json

        subroutine function_def_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(function_def_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine function_def_to_json

        subroutine subroutine_def_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(subroutine_def_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine subroutine_def_to_json

        subroutine function_call_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(function_call_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine function_call_to_json

        subroutine call_or_subscript_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(call_or_subscript_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine call_or_subscript_to_json

        subroutine identifier_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(identifier_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine identifier_to_json

        subroutine literal_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(literal_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine literal_to_json

        subroutine use_statement_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(use_statement_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine use_statement_to_json

        subroutine include_statement_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(include_statement_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine include_statement_to_json

        subroutine print_statement_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(print_statement_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine print_statement_to_json

        subroutine declaration_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(declaration_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine declaration_to_json

        subroutine do_loop_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(do_loop_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine do_loop_to_json

        subroutine do_while_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(do_while_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine do_while_to_json

        subroutine if_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(if_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine if_to_json

        subroutine select_case_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(select_case_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine select_case_to_json

        subroutine derived_type_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(derived_type_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine derived_type_to_json

        subroutine interface_block_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(interface_block_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine interface_block_to_json

        subroutine module_to_json(this, json, parent)
            use json_module
            import :: ast_node
            class(module_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine module_to_json
    end interface

end module ast_types
