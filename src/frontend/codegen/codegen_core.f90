module codegen_core
    use ast_core
    use ast_lazy_fortran
    implicit none
    private
    
    ! Public interface for code generation
    public :: generate_code, generate_code_polymorphic
    
    ! Generic interface for all AST node types
    interface generate_code
        module procedure generate_code_literal
        module procedure generate_code_identifier
        module procedure generate_code_assignment
        module procedure generate_code_binary_op
        module procedure generate_code_program
        module procedure generate_code_function_def
        module procedure generate_code_subroutine_def
        module procedure generate_code_function_call
        module procedure generate_code_use_statement
        module procedure generate_code_print_statement
        ! Postmodern Fortran specific
        module procedure generate_code_lf_program
        module procedure generate_code_lf_assignment
        module procedure generate_code_inferred_var
    end interface generate_code

contains


    ! Generate code for literal node
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        ! Simply return the literal value
        code = node%value
    end function generate_code_literal

    ! Generate code for identifier node
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        ! Simply return the identifier name
        code = node%name
    end function generate_code_identifier

    ! Generate code for assignment node
    function generate_code_assignment(node) result(code)
        type(assignment_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: target_code, value_code
        
        ! Generate code for target and value
        select type (target => node%target)
        type is (identifier_node)
            target_code = generate_code_identifier(target)
        class default
            target_code = "???"
        end select
        
        select type (value => node%value)
        type is (literal_node)
            value_code = generate_code_literal(value)
        type is (identifier_node)
            value_code = generate_code_identifier(value)
        type is (binary_op_node)
            value_code = generate_code_binary_op(value)
        type is (function_call_node)
            value_code = generate_code_function_call(value)
        class default
            value_code = "???"
        end select
        
        ! Combine with assignment operator
        code = target_code // " = " // value_code
    end function generate_code_assignment

    ! Generate code for binary operation node
    recursive function generate_code_binary_op(node) result(code)
        type(binary_op_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code
        
        ! Generate code for left and right operands
        select type (left => node%left)
        type is (literal_node)
            left_code = generate_code_literal(left)
        type is (identifier_node)
            left_code = generate_code_identifier(left)
        type is (binary_op_node)
            left_code = generate_code_binary_op(left)
        type is (function_call_node)
            left_code = generate_code_function_call(left)
        class default
            left_code = "???"
        end select
        
        select type (right => node%right)
        type is (literal_node)
            right_code = generate_code_literal(right)
        type is (identifier_node)
            right_code = generate_code_identifier(right)
        type is (binary_op_node)
            right_code = generate_code_binary_op(right)
        type is (function_call_node)
            right_code = generate_code_function_call(right)
        class default
            right_code = "???"
        end select
        
        ! Combine with operator
        code = left_code // " " // node%operator // " " // right_code
    end function generate_code_binary_op

    ! Generate code for program node
    function generate_code_program(node) result(code)
        type(program_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        
        ! Start with program declaration
        code = "program " // node%name // new_line('a')
        code = code // "    implicit none" // new_line('a')
        
        ! Generate code for each statement in the body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                ! This would need to handle each statement type
                code = code // "    ! statement" // new_line('a')
            end do
        end if
        
        ! End program
        code = code // "end program " // node%name
    end function generate_code_program

    ! Generate code for function definition
    function generate_code_function_def(node) result(code)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        code = "function " // node%name // "()"
    end function generate_code_function_def

    ! Generate code for subroutine definition
    function generate_code_subroutine_def(node) result(code)
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        code = "subroutine " // node%name // "()"
    end function generate_code_subroutine_def

    ! Generate code for function call
    recursive function generate_code_function_call(node) result(code)
        type(function_call_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i
        
        code = node%name // "("
        
        ! Generate arguments
        if (allocated(node%args)) then
            do i = 1, size(node%args)
                if (i > 1) code = code // ", "
                
                select type (arg => node%args(i))
                type is (literal_node)
                    code = code // generate_code_literal(arg)
                type is (identifier_node)
                    code = code // generate_code_identifier(arg)
                type is (binary_op_node)
                    code = code // generate_code_binary_op(arg)
                type is (function_call_node)
                    code = code // generate_code_function_call(arg)
                class default
                    code = code // "?"
                end select
            end do
        end if
        
        code = code // ")"
    end function generate_code_function_call

    ! Generate code for use statement
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        code = "use " // node%module_name
    end function generate_code_use_statement

    ! Generate code for print statement
    function generate_code_print_statement(node) result(code)
        type(print_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i
        
        code = "print *"
        
        ! Add arguments if present
        if (allocated(node%args)) then
            if (size(node%args) > 0) then
                ! For now, just handle simple case
                code = "print *, result"
            end if
        end if
    end function generate_code_print_statement

    ! Generate code for Simple Fortran program node
    function generate_code_lf_program(node) result(code)
        type(lf_program_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        
        ! Start with program declaration
        code = "program " // node%name // new_line('a')
        code = code // "    implicit none" // new_line('a')
        
        ! Add variable declarations based on inferred types
        ! (This would be done by analyzing the AST)
        
        ! Generate code for each statement in the body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i))
                type is (assignment_node)
                    code = code // "    " // generate_code(stmt) // new_line('a')
                type is (lf_assignment_node)
                    code = code // "    " // generate_code(stmt) // new_line('a')
                class default
                    ! Handle other statement types
                    code = code // "    ! statement" // new_line('a')
                end select
            end do
        end if
        
        ! End program
        code = code // "end program " // node%name
    end function generate_code_lf_program

    ! Generate code for Simple Fortran assignment (with type inference)
    function generate_code_lf_assignment(node) result(code)
        type(lf_assignment_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        ! For now, just generate as regular assignment
        ! In the future, we'd handle type declarations here
        code = generate_code_assignment(node%assignment_node)
    end function generate_code_lf_assignment

    ! Generate code for inferred variable
    function generate_code_inferred_var(node) result(code)
        type(inferred_var_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        ! Just return the variable name
        code = node%name
    end function generate_code_inferred_var

    ! Polymorphic dispatcher for class(ast_node)
    function generate_code_polymorphic(node) result(code)
        class(ast_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        select type (node)
        type is (literal_node)
            code = generate_code_literal(node)
        type is (identifier_node)
            code = generate_code_identifier(node)
        type is (assignment_node)
            code = generate_code_assignment(node)
        type is (binary_op_node)
            code = generate_code_binary_op(node)
        type is (function_call_node)
            code = generate_code_function_call(node)
        type is (lf_assignment_node)
            code = generate_code_lf_assignment(node)
        class default
            code = "! Unknown AST node type"
        end select
    end function generate_code_polymorphic

end module codegen_core