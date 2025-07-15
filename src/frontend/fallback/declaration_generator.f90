module declaration_generator
    ! FALLBACK MODULE: Variable declaration generation from tokens
    ! TODO: Remove this entire module when full AST with type inference is implemented
    ! This module generates variable declarations by parsing tokens and inferring types
    
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_IDENTIFIER
    use parser_core, only: parse_statement, parser_state_t, create_parser_state
    use ast_core
    use ast_lazy_fortran
    use semantic_analyzer, only: semantic_context_t
    use token_fallback, only: get_function_names_from_tokens, current_tokens
    implicit none
    private
    
    ! Public interface
    public :: generate_declarations
    public :: infer_basic_type
    public :: is_function_name
    
contains

    ! Generate variable declarations from AST with type info
    function generate_declarations(prog, sem_ctx) result(decls)
        ! Generate declarations from AST body
        type(lf_program_node), intent(in) :: prog
        type(semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable :: decls
        character(len=:), allocatable :: type_str, var_name
        character(len=:), allocatable :: function_names
        character(len=256), allocatable :: var_names(:)
        logical, allocatable :: var_declared(:)
        integer :: n_vars, i, j
        
        decls = ""
        n_vars = 0
        
        ! Get list of all function names to avoid declaring them as variables
        function_names = get_function_names_from_tokens()
        
        ! First pass: count variables
        if (allocated(prog%body)) then
            do i = 1, size(prog%body)
                if (allocated(prog%body(i)%node)) then
                    select type (stmt => prog%body(i)%node)
                    type is (assignment_node)
                        n_vars = n_vars + 1
                    end select
                end if
            end do
        end if
        
        if (n_vars == 0) return
        
        ! Allocate arrays to track variables
        allocate(var_names(n_vars))
        allocate(var_declared(n_vars))
        var_declared = .false.
        
        ! Second pass: collect variables and generate declarations
        n_vars = 0
        if (allocated(prog%body)) then
            do i = 1, size(prog%body)
                if (allocated(prog%body(i)%node)) then
                    select type (stmt => prog%body(i)%node)
                    type is (assignment_node)
                        ! Get variable name
                        select type (target => stmt%target)
                        type is (identifier_node)
                            var_name = target%name
                            
                            ! Check if already declared
                            block
                                logical :: already_declared
                                already_declared = .false.
                                do j = 1, n_vars
                                    if (var_names(j) == var_name) then
                                        already_declared = .true.
                                        exit
                                    end if
                                end do
                                
                                if (.not. already_declared .and. &
                                    .not. is_function_name(var_name, function_names)) then
                                    n_vars = n_vars + 1
                                    var_names(n_vars) = var_name
                                    
                                    ! Basic type inference from assignment value
                                    type_str = infer_basic_type(stmt%value)
                                    decls = decls // "    " // type_str // " :: " // var_name // new_line('a')
                                end if
                            end block
                        end select
                    end select
                end if
            end do
        end if
        
    end function generate_declarations

    ! Basic type inference from assignment value
    recursive function infer_basic_type(value_node) result(type_str)
        class(ast_node), intent(in) :: value_node
        character(len=:), allocatable :: type_str
        
        select type (value_node)
        type is (literal_node)
            select case (value_node%literal_kind)
            case (LITERAL_INTEGER)
                type_str = "integer"
            case (LITERAL_REAL)
                type_str = "real(8)"
            case (LITERAL_STRING)
                ! Use fixed length for now - proper length inference needed
                type_str = "character(len=256)"
            case (LITERAL_LOGICAL)
                type_str = "logical"
            case default
                error stop "Type inference failed: Unknown literal kind"
            end select
        type is (binary_op_node)
            ! For binary operations, infer from left operand
            type_str = infer_basic_type(value_node%left)
        type is (identifier_node)
            ! Cannot infer type from identifier alone
            error stop "Type inference failed: Cannot infer type for identifier '" // trim(value_node%name) // "'"
        type is (function_call_node)
            ! Try to infer from function name
            select case (trim(value_node%name))
            case ("sin", "cos", "tan", "exp", "log", "sqrt", "abs", "acos", "asin", "atan")
                type_str = "real(8)"
            case ("len", "len_trim", "index", "size", "count", "nint", "int")
                type_str = "integer"
            case default
                ! Unknown function
                error stop "Type inference failed: Unknown function '" // trim(value_node%name) // "'"
            end select
        class default
            ! Unknown node type
            error stop "Type inference failed: Unknown AST node type"
        end select
    end function infer_basic_type

    ! Check if a given name is in comma-separated list of function names
    function is_function_name(name, func_names_list) result(is_func)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: func_names_list
        logical :: is_func
        
        integer :: pos, start_pos, end_pos
        character(len=:), allocatable :: current_name
        
        is_func = .false.
        
        if (len_trim(func_names_list) == 0) return
        
        ! Search through comma-separated list
        start_pos = 1
        do
            ! Find next comma or end of string
            pos = index(func_names_list(start_pos:), ",")
            if (pos == 0) then
                end_pos = len_trim(func_names_list)
            else
                end_pos = start_pos + pos - 2
            end if
            
            ! Extract current function name
            if (end_pos >= start_pos) then
                current_name = trim(func_names_list(start_pos:end_pos))
                if (trim(current_name) == trim(name)) then
                    is_func = .true.
                    return
                end if
            end if
            
            ! Move to next name
            if (pos == 0) exit
            start_pos = start_pos + pos
        end do
    end function is_function_name

end module declaration_generator