module ast
    ! Unified AST interface that combines core and dialect-specific nodes
    use ast_core
    use ast_simple_fortran
    use json_module
    implicit none
    private

    ! Re-export all core AST node types
    public :: ast_node
    public :: program_node, assignment_node, binary_op_node
    public :: function_def_node, subroutine_def_node, function_call_node
    public :: identifier_node, literal_node, use_statement_node, print_statement_node
    
    ! Re-export Simple Fortran AST node types
    public :: sf_program_node, inferred_var_node, list_comp_node
    public :: fstring_node, sf_assignment_node
    
    ! Re-export literal kind constants
    public :: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    
    ! Re-export all factory functions
    public :: create_program, create_assignment, create_binary_op
    public :: create_function_def, create_subroutine_def, create_function_call
    public :: create_identifier, create_literal, create_use_statement, create_print_statement
    public :: create_sf_program, create_inferred_var, create_list_comp
    public :: create_fstring, create_sf_assignment
    
    ! AST manipulation utilities
    public :: ast_to_json_file, ast_to_json_string
    public :: ast_visitor_base

    ! Base visitor type for implementing visitor pattern
    type, abstract, public :: ast_visitor_base
    contains
        procedure(visit_program_interface), deferred :: visit_program
        procedure(visit_assignment_interface), deferred :: visit_assignment
        procedure(visit_binary_op_interface), deferred :: visit_binary_op
        procedure(visit_function_def_interface), deferred :: visit_function_def
        procedure(visit_subroutine_def_interface), deferred :: visit_subroutine_def
        procedure(visit_function_call_interface), deferred :: visit_function_call
        procedure(visit_identifier_interface), deferred :: visit_identifier
        procedure(visit_literal_interface), deferred :: visit_literal
        procedure(visit_use_statement_interface), deferred :: visit_use_statement
        procedure(visit_print_statement_interface), deferred :: visit_print_statement
        procedure(visit_sf_program_interface), deferred :: visit_sf_program
        procedure(visit_inferred_var_interface), deferred :: visit_inferred_var
        procedure(visit_list_comp_interface), deferred :: visit_list_comp
        procedure(visit_fstring_interface), deferred :: visit_fstring
        procedure(visit_sf_assignment_interface), deferred :: visit_sf_assignment
    end type ast_visitor_base

    ! Abstract interfaces for visitor methods
    abstract interface
        subroutine visit_program_interface(this, node)
            import :: ast_visitor_base, program_node
            class(ast_visitor_base), intent(inout) :: this
            type(program_node), intent(in) :: node
        end subroutine visit_program_interface

        subroutine visit_assignment_interface(this, node)
            import :: ast_visitor_base, assignment_node
            class(ast_visitor_base), intent(inout) :: this
            type(assignment_node), intent(in) :: node
        end subroutine visit_assignment_interface

        subroutine visit_binary_op_interface(this, node)
            import :: ast_visitor_base, binary_op_node
            class(ast_visitor_base), intent(inout) :: this
            type(binary_op_node), intent(in) :: node
        end subroutine visit_binary_op_interface

        subroutine visit_function_def_interface(this, node)
            import :: ast_visitor_base, function_def_node
            class(ast_visitor_base), intent(inout) :: this
            type(function_def_node), intent(in) :: node
        end subroutine visit_function_def_interface

        subroutine visit_subroutine_def_interface(this, node)
            import :: ast_visitor_base, subroutine_def_node
            class(ast_visitor_base), intent(inout) :: this
            type(subroutine_def_node), intent(in) :: node
        end subroutine visit_subroutine_def_interface

        subroutine visit_function_call_interface(this, node)
            import :: ast_visitor_base, function_call_node
            class(ast_visitor_base), intent(inout) :: this
            type(function_call_node), intent(in) :: node
        end subroutine visit_function_call_interface

        subroutine visit_identifier_interface(this, node)
            import :: ast_visitor_base, identifier_node
            class(ast_visitor_base), intent(inout) :: this
            type(identifier_node), intent(in) :: node
        end subroutine visit_identifier_interface

        subroutine visit_literal_interface(this, node)
            import :: ast_visitor_base, literal_node
            class(ast_visitor_base), intent(inout) :: this
            type(literal_node), intent(in) :: node
        end subroutine visit_literal_interface

        subroutine visit_use_statement_interface(this, node)
            import :: ast_visitor_base, use_statement_node
            class(ast_visitor_base), intent(inout) :: this
            type(use_statement_node), intent(in) :: node
        end subroutine visit_use_statement_interface

        subroutine visit_print_statement_interface(this, node)
            import :: ast_visitor_base, print_statement_node
            class(ast_visitor_base), intent(inout) :: this
            type(print_statement_node), intent(in) :: node
        end subroutine visit_print_statement_interface

        subroutine visit_sf_program_interface(this, node)
            import :: ast_visitor_base, sf_program_node
            class(ast_visitor_base), intent(inout) :: this
            type(sf_program_node), intent(in) :: node
        end subroutine visit_sf_program_interface

        subroutine visit_inferred_var_interface(this, node)
            import :: ast_visitor_base, inferred_var_node
            class(ast_visitor_base), intent(inout) :: this
            type(inferred_var_node), intent(in) :: node
        end subroutine visit_inferred_var_interface

        subroutine visit_list_comp_interface(this, node)
            import :: ast_visitor_base, list_comp_node
            class(ast_visitor_base), intent(inout) :: this
            type(list_comp_node), intent(in) :: node
        end subroutine visit_list_comp_interface

        subroutine visit_fstring_interface(this, node)
            import :: ast_visitor_base, fstring_node
            class(ast_visitor_base), intent(inout) :: this
            type(fstring_node), intent(in) :: node
        end subroutine visit_fstring_interface

        subroutine visit_sf_assignment_interface(this, node)
            import :: ast_visitor_base, sf_assignment_node
            class(ast_visitor_base), intent(inout) :: this
            type(sf_assignment_node), intent(in) :: node
        end subroutine visit_sf_assignment_interface
    end interface

contains

    ! Write AST to JSON file
    subroutine ast_to_json_file(root, filename)
        class(ast_node), intent(in) :: root
        character(len=*), intent(in) :: filename
        
        type(json_core) :: json
        type(json_value), pointer :: json_root
        
        ! Initialize JSON
        call json%initialize()
        
        ! Create root object
        call json%create_object(json_root, '')
        
        ! Convert AST to JSON
        call root%to_json(json, json_root)
        
        ! Write to file
        call json%print(json_root, filename)
        
        ! Clean up
        call json%destroy(json_root)
    end subroutine ast_to_json_file

    ! Convert AST to JSON string
    function ast_to_json_string(root) result(json_str)
        class(ast_node), intent(in) :: root
        character(len=:), allocatable :: json_str
        
        type(json_core) :: json
        type(json_value), pointer :: json_root
        
        ! Initialize JSON
        call json%initialize()
        
        ! Create root object
        call json%create_object(json_root, '')
        
        ! Convert AST to JSON
        call root%to_json(json, json_root)
        
        ! Convert to string
        call json%print_to_string(json_root, json_str)
        
        ! Clean up
        call json%destroy(json_root)
    end function ast_to_json_string

end module ast