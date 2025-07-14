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
        
        ! Return the literal value with proper formatting
        select case (node%literal_kind)
        case (LITERAL_STRING)
            ! String literals need quotes if not already present
            if (len(node%value) > 0 .and. node%value(1:1) /= '"' .and. node%value(1:1) /= "'") then
                code = '"' // node%value // '"'
            else
                code = node%value
            end if
        case (LITERAL_REAL)
            ! For real literals, ensure double precision by adding 'd0' suffix if needed
            if (index(node%value, 'd') == 0 .and. index(node%value, 'D') == 0 .and. &
                index(node%value, '_') == 0) then
                code = node%value // "d0"
            else
                code = node%value
            end if
        case default
            ! Handle invalid/empty literals safely
            if (allocated(node%value) .and. len_trim(node%value) > 0) then
                code = node%value
            else
                code = "! Invalid literal node"
            end if
        end select
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
        character(len=:), allocatable :: function_interfaces
        logical :: has_functions, has_contains
        
        ! Start with program declaration
        code = "program " // node%name // new_line('a')
        
        ! Generate use statements first (must come before implicit none)
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    code = code // "    " // generate_code(stmt) // new_line('a')
                end select
            end do
        end if
        
        code = code // "    implicit none" // new_line('a')
        
        ! Analyze for function calls and generate interface blocks
        function_interfaces = ""
        has_functions = .false.
        has_contains = .false.
        
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (assignment_node)
                    ! Check if assignment contains function calls
                    block
                        character(len=:), allocatable :: interface_code
                        interface_code = analyze_for_function_calls(stmt)
                        if (len_trim(interface_code) > 0) then
                            function_interfaces = function_interfaces // interface_code // new_line('a')
                            has_functions = .true.
                        end if
                    end block
                end select
            end do
        end if
        
        ! Add interface blocks if needed
        if (has_functions) then
            code = code // new_line('a') // "    interface" // new_line('a')
            code = code // function_interfaces
            code = code // "    end interface" // new_line('a') // new_line('a')
        end if
        
        ! Generate code for each statement in the body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    ! Skip - already generated before implicit none
                type is (assignment_node)
                    code = code // "    " // generate_code(stmt) // new_line('a')
                type is (function_def_node)
                    if (.not. has_contains) then
                        code = code // "contains" // new_line('a')
                        has_contains = .true.
                    end if
                    code = code // "    " // generate_code(stmt) // new_line('a')
                class default
                    ! Use polymorphic dispatcher for other types
                    code = code // "    " // generate_code_polymorphic(stmt) // new_line('a')
                end select
            end do
        end if
        
        ! End program
        code = code // "end program " // node%name
    end function generate_code_program

    ! Generate code for function definition
    function generate_code_function_def(node) result(code)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        
        ! Start function declaration
        code = "function " // node%name // "("
        
        ! Add parameters if present
        if (allocated(node%params)) then
            do i = 1, size(node%params)
                if (i > 1) code = code // ", "
                select type (param => node%params(i))
                type is (identifier_node)
                    code = code // param%name
                class default
                    code = code // "param" // char(i + ichar('0'))
                end select
            end do
        end if
        
        code = code // ")" // new_line('a')
        code = code // "    implicit none" // new_line('a')
        
        ! Add parameter declarations (simplified for now)
        if (allocated(node%params)) then
            do i = 1, size(node%params)
                select type (param => node%params(i))
                type is (identifier_node)
                    code = code // "    real(8), intent(in) :: " // param%name // new_line('a')
                class default
                    code = code // "    real(8), intent(in) :: param" // char(i + ichar('0')) // new_line('a')
                end select
            end do
        end if
        
        ! Add return type declaration
        code = code // "    real(8) :: " // node%name // new_line('a')
        
        ! Add function body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                code = code // "    " // generate_code_polymorphic(node%body(i)) // new_line('a')
            end do
        end if
        
        ! End function
        code = code // "end function " // node%name
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
                
                select type (arg => node%args(i)%node)
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
        
        ! Start with format spec
        if (allocated(node%format_spec) .and. len_trim(node%format_spec) > 0) then
            code = "print " // node%format_spec
        else
            code = "print *"
        end if
        
        ! Add arguments if present
        if (allocated(node%args) .and. size(node%args) > 0) then
            code = code // ", "
            do i = 1, size(node%args)
                if (i > 1) code = code // ", "
                
                select type (arg => node%args(i)%node)
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
    end function generate_code_print_statement

    ! Generate code for Simple Fortran program node
    function generate_code_lf_program(node) result(code)
        type(lf_program_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=:), allocatable :: function_interfaces
        logical :: has_functions, has_contains
        
        ! Start with program declaration
        code = "program " // node%name // new_line('a')
        
        ! Generate use statements first (must come before implicit none)
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    code = code // "    " // generate_code(stmt) // new_line('a')
                end select
            end do
        end if
        
        code = code // "    implicit none" // new_line('a')
        
        ! Add variable declarations based on inferred types
        block
            character(len=:), allocatable :: var_declarations
            var_declarations = analyze_for_variable_declarations(node)
            if (len_trim(var_declarations) > 0) then
                code = code // var_declarations // new_line('a')
            end if
        end block
        
        ! Analyze for function calls and generate interface blocks
        function_interfaces = ""
        has_functions = .false.
        has_contains = .false.
        
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (assignment_node)
                    ! Check if assignment contains function calls
                    block
                        character(len=:), allocatable :: interface_code
                        interface_code = analyze_for_function_calls(stmt)
                        if (len_trim(interface_code) > 0) then
                            function_interfaces = function_interfaces // interface_code // new_line('a')
                            has_functions = .true.
                        end if
                    end block
                end select
            end do
        end if
        
        ! Add interface blocks if needed
        if (has_functions) then
            code = code // new_line('a') // "    interface" // new_line('a')
            code = code // function_interfaces
            code = code // "    end interface" // new_line('a') // new_line('a')
        end if
        
        ! Generate code for each statement in the body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    ! Skip - already generated before implicit none
                type is (assignment_node)
                    code = code // "    " // generate_code(stmt) // new_line('a')
                type is (lf_assignment_node)
                    code = code // "    " // generate_code(stmt) // new_line('a')
                type is (function_def_node)
                    if (.not. has_contains) then
                        code = code // new_line('a') // "contains" // new_line('a')
                        has_contains = .true.
                    end if
                    code = code // new_line('a') // generate_code(stmt) // new_line('a')
                class default
                    ! Use polymorphic dispatcher for other types
                    code = code // "    " // generate_code_polymorphic(stmt) // new_line('a')
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
        type is (function_def_node)
            code = generate_code_function_def(node)
        type is (subroutine_def_node)
            code = generate_code_subroutine_def(node)
        type is (lf_assignment_node)
            code = generate_code_lf_assignment(node)
        type is (print_statement_node)
            code = generate_code_print_statement(node)
        type is (use_statement_node)
            code = generate_code_use_statement(node)
        type is (inferred_var_node)
            code = generate_code_inferred_var(node)
        class default
            ! NEVER generate "0" - always generate a comment for debugging
            code = "! Unimplemented AST node"
        end select
    end function generate_code_polymorphic

    ! Analyze assignment for function calls and generate interface declarations
    recursive function analyze_for_function_calls(stmt) result(interface_code)
        type(assignment_node), intent(in) :: stmt
        character(len=:), allocatable :: interface_code
        
        interface_code = ""
        
        ! Check the value side of assignment for function calls
        select type (value => stmt%value)
        type is (function_call_node)
            ! Generate interface for this function call
            interface_code = "        function " // value%name // "("
            if (allocated(value%args) .and. size(value%args) > 0) then
                interface_code = interface_code // "x"  ! Simplified parameter
            end if
            interface_code = interface_code // ")" // new_line('a')
            interface_code = interface_code // "            real(8) :: " // value%name // new_line('a')
            interface_code = interface_code // "            real(8), intent(in) :: x" // new_line('a')
            interface_code = interface_code // "        end function " // value%name
        type is (binary_op_node)
            ! Recursively check binary operations for function calls
            interface_code = analyze_binary_op_for_functions(value)
        end select
        
    end function analyze_for_function_calls
    
    ! Recursively analyze binary operations for function calls
    recursive function analyze_binary_op_for_functions(binop) result(interface_code)
        type(binary_op_node), intent(in) :: binop
        character(len=:), allocatable :: interface_code
        
        interface_code = ""
        
        ! Check left operand
        select type (left => binop%left)
        type is (function_call_node)
            interface_code = "        function " // left%name // "("
            if (allocated(left%args) .and. size(left%args) > 0) then
                interface_code = interface_code // "x"
            end if
            interface_code = interface_code // ")" // new_line('a')
            interface_code = interface_code // "            real(8) :: " // left%name // new_line('a')
            interface_code = interface_code // "            real(8), intent(in) :: x" // new_line('a')
            interface_code = interface_code // "        end function " // left%name
        type is (binary_op_node)
            interface_code = interface_code // analyze_binary_op_for_functions(left)
        end select
        
        ! Check right operand
        select type (right => binop%right)
        type is (function_call_node)
            if (len_trim(interface_code) > 0) interface_code = interface_code // new_line('a')
            interface_code = interface_code // "        function " // right%name // "("
            if (allocated(right%args) .and. size(right%args) > 0) then
                interface_code = interface_code // "x"
            end if
            interface_code = interface_code // ")" // new_line('a')
            interface_code = interface_code // "            real(8) :: " // right%name // new_line('a')
            interface_code = interface_code // "            real(8), intent(in) :: x" // new_line('a')
            interface_code = interface_code // "        end function " // right%name
        type is (binary_op_node)
            block
                character(len=:), allocatable :: right_interface
                right_interface = analyze_binary_op_for_functions(right)
                if (len_trim(right_interface) > 0) then
                    if (len_trim(interface_code) > 0) interface_code = interface_code // new_line('a')
                    interface_code = interface_code // right_interface
                end if
            end block
        end select
        
    end function analyze_binary_op_for_functions

    ! Analyze program for variable declarations needed
    function analyze_for_variable_declarations(prog) result(declarations)
        type(lf_program_node), intent(in) :: prog
        character(len=:), allocatable :: declarations
        character(len=:), allocatable :: var_list
        integer :: i
        
        declarations = ""
        var_list = ""
        
        ! Analyze all statements to find variables that need declarations
        if (allocated(prog%body)) then
            do i = 1, size(prog%body)
                select type (stmt => prog%body(i)%node)
                type is (assignment_node)
                    ! Check target variable
                    select type (target => stmt%target)
                    type is (identifier_node)
                        if (index(var_list, target%name) == 0) then
                            if (len_trim(var_list) > 0) var_list = var_list // ","
                            var_list = var_list // target%name
                            
                            ! Simple type inference based on assigned value
                            block
                                character(len=:), allocatable :: var_type
                                var_type = "real(8)"  ! Default
                                
                                ! Check assignment value for type hints
                                select type (value => stmt%value)
                                type is (literal_node)
                                    if (value%literal_kind == LITERAL_INTEGER) then
                                        var_type = "integer"
                                    else if (value%literal_kind == LITERAL_REAL) then
                                        var_type = "real(8)"
                                    else if (value%literal_kind == LITERAL_STRING) then
                                        var_type = "character(len=*)"
                                    else if (value%literal_kind == LITERAL_LOGICAL) then
                                        var_type = "logical"
                                    end if
                                end select
                                
                                declarations = declarations // "    " // var_type // " :: " // target%name // new_line('a')
                            end block
                        end if
                    end select
                end select
            end do
        end if
        
    end function analyze_for_variable_declarations

end module codegen_core