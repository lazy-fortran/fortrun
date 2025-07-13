module declaration_generator
    ! FALLBACK MODULE: Variable declaration generation from tokens
    ! TODO: Remove this entire module when full AST with type inference is implemented
    ! This module generates variable declarations by parsing tokens and inferring types
    
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_IDENTIFIER
    use parser_core, only: parse_statement, parser_state_t, create_parser_state
    use ast_core
    use ast_lazy_fortran
    use semantic_analyzer_simple, only: simple_semantic_context_t
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
        ! Generate declarations from all statements using stored tokens
        type(lf_program_node), intent(in) :: prog
        type(simple_semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable :: decls
        character(len=:), allocatable :: type_str, var_name
        character(len=:), allocatable :: function_names
        
        ! Parse all statements from stored tokens for type inference
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt
        
        decls = ""
        
        ! Get list of all function names to avoid declaring them as variables
        function_names = get_function_names_from_tokens()
        
        ! Process all statements from stored tokens
        if (allocated(current_tokens)) then
            parser = create_parser_state(current_tokens)
            
            ! Parse statements by finding EOF boundaries
            do while (parser%current_token <= size(current_tokens))
                ! Find the end of the current statement (next EOF or end of tokens)
                block
                    integer :: stmt_start, stmt_end, j
                    type(token_t), allocatable :: stmt_tokens(:)
                    
                    stmt_start = parser%current_token
                    stmt_end = stmt_start
                    
                    ! Find next EOF token to determine statement boundary
                    do j = stmt_start, size(current_tokens)
                        if (current_tokens(j)%kind == TK_EOF) then
                            stmt_end = j - 1
                            exit
                        end if
                        stmt_end = j
                    end do
                    
                    ! Extract tokens for this statement
                    if (stmt_end >= stmt_start) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Parse this statement
                        stmt = parse_statement(stmt_tokens)
                        
                        if (allocated(stmt)) then
                            ! Extract declarations from assignment statements
                            select type (stmt)
                            type is (assignment_node)
                                ! Get variable name
                                select type (target => stmt%target)
                                type is (identifier_node)
                                    var_name = target%name
                                    
                                    ! Check if this variable name is actually a function name
                                    if (.not. is_function_name(var_name, function_names)) then
                                        ! Basic type inference from assignment value
                                        type_str = infer_basic_type(stmt%value)
                                        decls = decls // "    " // type_str // " :: " // var_name // new_line('a')
                                    end if
                                end select
                            end select
                        end if
                    end if
                    
                    ! Move to next statement (skip past EOF)
                    parser%current_token = stmt_end + 2  ! Skip EOF token
                end block
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
                type_str = "character(len=*)"
            case (LITERAL_LOGICAL)
                type_str = "logical"
            case default
                type_str = "real(8)"  ! Default fallback
            end select
        type is (binary_op_node)
            ! For binary operations, infer from left operand
            type_str = infer_basic_type(value_node%left)
        type is (identifier_node)
            ! For identifiers, default to real(8) (lazy fortran default)
            type_str = "real(8)"
        type is (function_call_node)
            ! Function calls default to real(8)
            type_str = "real(8)"
        class default
            ! Default fallback
            type_str = "real(8)"
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