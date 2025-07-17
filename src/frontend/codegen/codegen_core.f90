module codegen_core
    use ast_core
    use type_system_hm
    use codegen_declarations
    implicit none
    private

    ! Context for function indentation
    logical :: context_has_executable_before_contains = .false.

    ! Public interface for code generation
    public :: generate_code, generate_code_polymorphic, generate_code_declaration

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
        module procedure generate_code_declaration
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
            if (len_trim(node%value) == 0) then
                code = ""  ! Skip empty literals (parser placeholders)
            else if (len(node%value) > 0 .and. node%value(1:1) /= '"' .and. &
                     node%value(1:1) /= "'") then
                code = '"'//node%value//'"'
            else
                code = node%value
            end if
        case (LITERAL_REAL)
            ! For real literals, ensure double precision by adding 'd0' suffix if needed
            if (index(node%value, 'd') == 0 .and. index(node%value, 'D') == 0 .and. &
                index(node%value, '_') == 0) then
                code = node%value//"d0"
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
        code = target_code//" = "//value_code
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

        ! Combine with operator - different spacing based on context
        if (node%operator == "*" .or. node%operator == "/") then
            ! Use spaces for * and / in functions without executable before contains
            if (.not. context_has_executable_before_contains) then
                code = left_code//" "//node%operator//" "//right_code  ! Spaces
            else
                code = left_code//node%operator//right_code  ! No spaces
            end if
        else
            code = left_code//" "//node%operator//" "//right_code  ! Always spaces for + and -
        end if
    end function generate_code_binary_op

    ! Generate code for program node
    function generate_code_program(node) result(code)
        type(program_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=:), allocatable :: function_interfaces
        logical :: has_functions, has_contains, has_executable_before_contains

        ! Start with program declaration
        code = "program "//node%name//new_line('a')

        ! Generate use statements first (must come before implicit none)
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    code = code//"    "//generate_code(stmt)//new_line('a')
                end select
            end do
        end if

        code = code//"    implicit none"//new_line('a')

        ! Add variable declarations using semantic analyzer's inferred types
        block
            character(len=:), allocatable :: var_declarations
            var_declarations = generate_variable_declarations(node)
            if (len_trim(var_declarations) > 0) then
                code = code//var_declarations//new_line('a')
            end if
        end block

        ! STAGE 2 WORKAROUND: Disable interface generation completely to avoid conflicts
        ! Interface blocks are not needed for internal functions
        function_interfaces = ""
        has_functions = .false.
        has_contains = .false.
        has_executable_before_contains = .false.

        ! Generate code for each statement in the body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    ! Skip - already generated before implicit none
                type is (assignment_node)
                    has_executable_before_contains = .true.
                    code = code//"    "//generate_code(stmt)//new_line('a')
                type is (function_def_node)
                    if (.not. has_contains) then
                        code = code//"contains"//new_line('a')
                        has_contains = .true.
                    end if
                    ! Set context for function indentation
                 context_has_executable_before_contains = has_executable_before_contains
                    code = code//generate_code(stmt)//new_line('a')
                class default
                    ! Use polymorphic dispatcher for other types
                    block
                        character(len=:), allocatable :: stmt_code
                        stmt_code = generate_code_polymorphic(stmt)
                        if (len_trim(stmt_code) > 0) then
                            code = code//"    "//stmt_code//new_line('a')
                        end if
                    end block
                end select
            end do
        end if
        ! End program
        code = code//"end program "//node%name
    end function generate_code_program

    ! Generate code for function definition
    function generate_code_function_def(node) result(code)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i

        ! Start function declaration with return type if present
        block
            character(len=:), allocatable :: return_type_str

            ! Get return type string
            if (allocated(node%return_type)) then
                select type (ret_type => node%return_type)
                type is (identifier_node)
                    if (len_trim(ret_type%name) > 0) then
                        ! Normalize real to real(8) for consistency
                        if (trim(ret_type%name) == "real") then
                            return_type_str = "real(8)"
                        else
                            return_type_str = ret_type%name
                        end if
                    else
                        return_type_str = "real(8)"  ! Default
                    end if
                class default
                    return_type_str = "real(8)"  ! Default
                end select
            else
                return_type_str = "real(8)"  ! Default
            end if

            ! STAGE 2 ENHANCEMENT: Enhanced function signature generation
            ! Generate function declaration with enhanced signature
            code = "    "//return_type_str//" function "//node%name//"("

            ! Add parameters if present
            if (allocated(node%params)) then
                do i = 1, size(node%params)
                    if (i > 1) code = code//", "
                    select type (param => node%params(i)%node)
                    type is (identifier_node)
                        code = code//param%name
                    class default
                        code = code//"param"//char(i + ichar('0'))
                    end select
                end do
            end if

            code = code//")"//new_line('a')
            if (context_has_executable_before_contains) then
                code = code//"        implicit none"//new_line('a')
            else
                code = code//"    implicit none"//new_line('a')
            end if

            ! STAGE 2 ENHANCEMENT: Enhanced parameter declarations with intent(in)
            if (allocated(node%params)) then
                ! Combine parameters of the same type onto one line
                block
                    character(len=:), allocatable :: param_names
                    integer :: param_count
                    param_count = 0
                    param_names = ""

                    do i = 1, size(node%params)
                        select type (param => node%params(i)%node)
                        type is (identifier_node)
                            param_count = param_count + 1
                            if (param_count > 1) then
                                param_names = param_names//", "
                            end if
                            param_names = param_names//param%name
                        end select
                    end do

                    if (param_count > 0) then
                        if (context_has_executable_before_contains) then
                            code = code//"        "//return_type_str// &
                                   ", intent(in) :: "//param_names//new_line('a')
                        else
                            code = code//"    "//return_type_str// &
                                   ", intent(in) :: "//param_names//new_line('a')
                        end if
                    end if
                end block
            end if

            ! STAGE 2 FIX: Don't redeclare function name when already in signature
            ! Function return type is already specified in the signature
            ! code = code // "    " // return_type_str // " :: " // &
            !        node%name // new_line('a')
        end block

        ! Add function body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                block
                    character(len=:), allocatable :: body_code
                    logical :: skip_statement
                    skip_statement = .false.

                    ! Check if this is a declaration for a parameter
                    select type (stmt => node%body(i)%node)
                    type is (declaration_node)
                        ! Skip declarations that match parameter names
                        if (allocated(node%params)) then
                            block
                                integer :: j
                                do j = 1, size(node%params)
                                    select type (param => node%params(j)%node)
                                    type is (identifier_node)
                                        if (stmt%var_name == param%name) then
                                            skip_statement = .true.
                                            exit
                                        end if
                                    end select
                                end do
                            end block
                        end if
                    end select

                    if (.not. skip_statement) then
                        body_code = generate_code_polymorphic(node%body(i)%node)
                        ! Skip empty or placeholder statements
                        if (len_trim(body_code) > 0 .and. &
                            body_code /= "! Function body statement") then
                            if (context_has_executable_before_contains) then
                                code = code//"        "//body_code//new_line('a')
                            else
                                code = code//"    "//body_code//new_line('a')
                            end if
                        end if
                    end if
                end block
            end do
        end if
        ! End function
        if (context_has_executable_before_contains) then
            code = code//"    end function "//node%name
        else
            code = code//"end function "//node%name
        end if
    end function generate_code_function_def

    ! Generate code for subroutine definition
    function generate_code_subroutine_def(node) result(code)
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: code

        code = "subroutine "//node%name//"()"
    end function generate_code_subroutine_def

    ! Generate code for function call
    recursive function generate_code_function_call(node) result(code)
        type(function_call_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        code = node%name//"("

        ! Generate arguments
        if (allocated(node%args)) then
            do i = 1, size(node%args)
                if (i > 1) code = code//", "

                select type (arg => node%args(i)%node)
                type is (literal_node)
                    code = code//generate_code_literal(arg)
                type is (identifier_node)
                    code = code//generate_code_identifier(arg)
                type is (binary_op_node)
                    code = code//generate_code_binary_op(arg)
                type is (function_call_node)
                    code = code//generate_code_function_call(arg)
                class default
                    code = code//"?"
                end select
            end do
        end if

        code = code//")"
    end function generate_code_function_call

    ! Generate code for use statement
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code

        code = "use "//node%module_name
    end function generate_code_use_statement

    ! Generate code for print statement
    function generate_code_print_statement(node) result(code)
        type(print_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        ! Start with format spec
        if (allocated(node%format_spec) .and. len_trim(node%format_spec) > 0) then
            code = "print "//node%format_spec
        else
            code = "print *"
        end if

        ! Add arguments if present
        if (allocated(node%args) .and. size(node%args) > 0) then
            code = code//", "
            do i = 1, size(node%args)
                if (i > 1) code = code//", "

                select type (arg => node%args(i)%node)
                type is (literal_node)
                    code = code//generate_code_literal(arg)
                type is (identifier_node)
                    code = code//generate_code_identifier(arg)
                type is (binary_op_node)
                    code = code//generate_code_binary_op(arg)
                type is (function_call_node)
                    code = code//generate_code_function_call(arg)
                class default
                    code = code//"?"
                end select
            end do
        end if
    end function generate_code_print_statement

    ! Generate code for declaration node
    function generate_code_declaration(node) result(code)
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: type_str, dimensions_str
        integer :: i

        ! Generate type declaration
        if (len_trim(node%type_name) > 0) then
            ! Use explicit type if provided
            type_str = node%type_name

            ! Add kind if specified
            if (node%has_kind) then
                if (node%type_name == "character") then
                    ! For character, use len= syntax
                    block
                        character(len=10) :: kind_str
                        write (kind_str, '(I0)') node%kind_value
                        type_str = type_str//"(len="//trim(adjustl(kind_str))//")"
                    end block
                else
                    ! For numeric types, use regular kind syntax
                    block
                        character(len=10) :: kind_str
                        write (kind_str, '(I0)') node%kind_value
                        type_str = type_str//"("//trim(adjustl(kind_str))//")"
                    end block
                end if
            else if (node%type_name == "real") then
                ! Default to real(8) for lazy fortran
                type_str = type_str//"(8)"
            end if
        else if (allocated(node%inferred_type)) then
            ! Use inferred type if no explicit type provided
            type_str = generate_type_declaration_from_mono(node%inferred_type)
        else
            ! Fallback
            type_str = "! ERROR: No type information"
        end if

        ! Start with allocatable attribute if present
        if (node%is_allocatable) then
            code = type_str//", allocatable :: "//node%var_name
        else
            code = type_str//" :: "//node%var_name
        end if

        ! Add array dimensions if present
        if (node%is_array .and. allocated(node%dimensions)) then
            dimensions_str = "("
            do i = 1, size(node%dimensions)
                if (i > 1) dimensions_str = dimensions_str//", "
                ! Generate code for each dimension
                if (allocated(node%dimensions(i)%node)) then
     dimensions_str = dimensions_str//generate_code_polymorphic(node%dimensions(i)%node)
                else
                    dimensions_str = dimensions_str//":"  ! Assumed shape
                end if
            end do
            dimensions_str = dimensions_str//")"
            code = code//dimensions_str
        end if

        ! Add initialization if present
        if (allocated(node%initializer)) then
            code = code//" = "//generate_code_polymorphic(node%initializer)
        end if
    end function generate_code_declaration

    ! Generate type declaration string from mono_type_t
    recursive function generate_type_declaration_from_mono(mono_type) result(type_str)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable :: type_str
        character(len=10) :: size_str

        select case (mono_type%kind)
        case (TINT)
            type_str = "integer"
        case (TREAL)
            type_str = "real(8)"  ! Default to real(8) for lazy fortran
        case (TCHAR)
            if (mono_type%size > 0) then
                write (size_str, '(I0)') mono_type%size
                type_str = "character(len="//trim(adjustl(size_str))//")"
            else
                type_str = "character(len=*)"  ! Assumed length
            end if
        case (TARRAY)
            ! For arrays, get the element type
            if (allocated(mono_type%args) .and. size(mono_type%args) > 0) then
                type_str = generate_type_declaration_from_mono(mono_type%args(1))
            else
                type_str = "! ERROR: Array without element type"
            end if
        case (TVAR)
            ! Type variable - should have been resolved by now
            type_str = "! ERROR: Unresolved type variable "//mono_type%var%name
        case (TFUN)
            ! Function type - not directly declarable in Fortran
            type_str = "! ERROR: Cannot declare function type directly"
        case default
            type_str = "! ERROR: Unknown type kind"
        end select
    end function generate_type_declaration_from_mono

    ! generate_code_lf_program removed - core program_node handler includes inference

    ! generate_code_lf_assignment removed - core assignment_node now has type inference

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
        type is (program_node)
            code = generate_code_program(node)
        type is (function_call_node)
            code = generate_code_function_call(node)
        type is (function_def_node)
            code = generate_code_function_def(node)
        type is (subroutine_def_node)
            code = generate_code_subroutine_def(node)
            ! lf_program_node now handled by program_node case through inheritance
        type is (print_statement_node)
            code = generate_code_print_statement(node)
        type is (use_statement_node)
            code = generate_code_use_statement(node)
        type is (declaration_node)
            code = generate_code_declaration(node)
        type is (do_loop_node)
            code = "! Do loop not implemented yet"
        type is (do_while_node)
            code = "! Do while not implemented yet"
        type is (select_case_node)
            code = "! Select case not implemented yet"
        class default
            ! NEVER generate "0" - always generate a comment for debugging
            code = "! Unimplemented AST node"
        end select
    end function generate_code_polymorphic

end module codegen_core
