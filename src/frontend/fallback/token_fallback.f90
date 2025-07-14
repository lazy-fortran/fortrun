module token_fallback
    ! FALLBACK MODULE: Token manipulation functions
    ! TODO: Remove this entire module when full AST support is implemented
    ! This module contains temporary token-to-code generation functions
    ! that bypass proper AST pipeline - use only until AST is complete
    
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_IDENTIFIER, TK_NEWLINE, TK_OPERATOR
    use parser_core, only: parse_statement, parser_state_t, create_parser_state
    use ast_core
    use codegen_core, only: generate_code_polymorphic
    implicit none
    private
    
    ! Module-level storage for current tokens (for multi-statement processing)
    type(token_t), allocatable, save :: current_tokens(:)
    
    ! Public interface - all FALLBACK functions
    public :: set_current_tokens, current_tokens
    public :: generate_use_statements_from_tokens
    public :: generate_executable_statements_from_tokens
    public :: generate_function_definitions_from_tokens
    public :: reconstruct_line_from_tokens
    public :: get_function_names_from_tokens
    public :: extract_function_name_from_tokens
    public :: is_function_def_statement
    
contains

    ! Store tokens for multi-statement processing
    subroutine set_current_tokens(tokens)
        type(token_t), intent(in) :: tokens(:)
        current_tokens = tokens
    end subroutine set_current_tokens
    
    ! FALLBACK: Generate USE statements from tokens until AST has USE nodes
    ! TODO: Remove when AST supports USE statement nodes
    function generate_use_statements_from_tokens() result(use_statements)
        character(len=:), allocatable :: use_statements
        integer :: i, stmt_start, stmt_end, current_line
        type(token_t), allocatable :: stmt_tokens(:)
        character(len=:), allocatable :: stmt_code
        
        use_statements = ""
        
        ! Simple token-based processing
        if (allocated(current_tokens)) then
            i = 1
            do while (i <= size(current_tokens))
                if (current_tokens(i)%kind == TK_EOF) exit
                
                ! Find statement boundaries (use line numbers to separate)
                stmt_start = i
                stmt_end = i
                
                ! Get all tokens on the same line
                current_line = current_tokens(i)%line
                
                do while (stmt_end <= size(current_tokens))
                    if (current_tokens(stmt_end)%kind == TK_EOF .or. &
                        current_tokens(stmt_end)%line > current_line) then
                        stmt_end = stmt_end - 1  ! Don't include the next line
                        exit
                    end if
                    stmt_end = stmt_end + 1
                end do
                
                ! Check if this is a USE statement
                if (stmt_end >= stmt_start .and. &
                    current_tokens(stmt_start)%kind == TK_KEYWORD .and. &
                    current_tokens(stmt_start)%text == "use") then
                    
                    allocate(stmt_tokens(stmt_end - stmt_start + 1))
                    stmt_tokens = current_tokens(stmt_start:stmt_end)
                    
                    stmt_code = reconstruct_line_from_tokens(stmt_tokens)
                    if (len_trim(stmt_code) > 0) then
                        use_statements = use_statements // "    " // trim(stmt_code) // new_line('a')
                    end if
                    
                    deallocate(stmt_tokens)
                end if
                
                i = stmt_end + 1
            end do
        end if
        
    end function generate_use_statements_from_tokens

    ! FALLBACK: Generate executable statements from tokens until AST has all statement types
    ! TODO: Remove when AST supports all statement node types
    ! SIMPLIFIED: Process line by line instead of complex token parsing
    function generate_executable_statements_from_tokens() result(statements)
        character(len=:), allocatable :: statements
        integer :: i, stmt_start, stmt_end, current_line
        type(token_t), allocatable :: stmt_tokens(:)
        character(len=:), allocatable :: stmt_code
        logical :: is_use_statement, is_function_definition
        
        statements = ""
        
        if (allocated(current_tokens)) then
            i = 1
            do while (i <= size(current_tokens))
                if (current_tokens(i)%kind == TK_EOF) exit
                
                ! Skip NEWLINE tokens
                if (current_tokens(i)%kind == TK_NEWLINE) then
                    i = i + 1
                    cycle
                end if
                
                ! Find statement boundaries (use line numbers to separate)
                stmt_start = i
                stmt_end = i
                
                ! Get all tokens on the same line
                current_line = current_tokens(i)%line
                
                do while (stmt_end <= size(current_tokens))
                    if (current_tokens(stmt_end)%kind == TK_EOF .or. &
                        current_tokens(stmt_end)%line > current_line) then
                        stmt_end = stmt_end - 1  ! Don't include the next line
                        exit
                    end if
                    stmt_end = stmt_end + 1
                end do
                
                if (stmt_end >= stmt_start) then
                    
                    ! Check if this is a USE statement
                    is_use_statement = .false.
                    if (current_tokens(stmt_start)%kind == TK_KEYWORD .and. &
                        current_tokens(stmt_start)%text == "use") then
                        is_use_statement = .true.
                    end if
                    
                    ! Check if this is a function definition
                    is_function_definition = .false.
                    if (stmt_start <= size(current_tokens)) then
                        if (current_tokens(stmt_start)%kind == TK_KEYWORD .and. &
                            current_tokens(stmt_start)%text == "function") then
                            is_function_definition = .true.
                        else if (is_function_def_statement(current_tokens(stmt_start:stmt_end))) then
                            is_function_definition = .true.
                        end if
                    end if
                    
                    ! Extract tokens for this statement if it's executable (not USE or function)
                    if (stmt_end >= stmt_start .and. .not. is_use_statement .and. .not. is_function_definition) then
                        ! Allocate tokens plus one for EOF
                        allocate(stmt_tokens(stmt_end - stmt_start + 2))
                        stmt_tokens(1:stmt_end - stmt_start + 1) = current_tokens(stmt_start:stmt_end)
                        ! Add EOF token at the end
                        stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                        stmt_tokens(stmt_end - stmt_start + 2)%text = ""
                        stmt_tokens(stmt_end - stmt_start + 2)%line = current_tokens(stmt_end)%line
                        stmt_tokens(stmt_end - stmt_start + 2)%column = current_tokens(stmt_end)%column + 1
                        
                        ! PROPER PIPELINE: Parse tokens → AST → Codegen
                        block
                            class(ast_node), allocatable :: stmt_ast
                            
                            ! Parse statement tokens to AST
                            stmt_ast = parse_statement(stmt_tokens)
                            
                            if (allocated(stmt_ast)) then
                                ! Generate code from AST using proper codegen
                                stmt_code = generate_code_polymorphic(stmt_ast)
                                if (len_trim(stmt_code) > 0) then
                                    statements = statements // "    " // trim(stmt_code) // char(10)
                                else
                                    ! FALLBACK: If AST exists but codegen returns empty, reconstruct
                                    ! Check if first token is print keyword
                                    if (size(stmt_tokens) > 0) then
                                        if (stmt_tokens(1)%kind == TK_KEYWORD .and. stmt_tokens(1)%text == "print") then
                                            ! Reconstruct print statement with proper formatting
                                            stmt_code = "print *"
                                            if (size(stmt_tokens) > 2) then
                                                stmt_code = stmt_code // ", "
                                                ! Add remaining tokens
                                                block
                                                    integer :: j
                                                    do j = 3, size(stmt_tokens) - 1  ! Skip EOF
                                                        if (stmt_tokens(j)%kind /= TK_EOF) then
                                                            if (j > 3 .and. stmt_tokens(j-1)%text == ",") then
                                                                stmt_code = stmt_code // " "
                                                            end if
                                                            stmt_code = stmt_code // trim(stmt_tokens(j)%text)
                                                        end if
                                                    end do
                                                end block
                                            end if
                                            statements = statements // "    " // trim(stmt_code) // char(10)
                                        else
                                            statements = statements // "    " // "0" // char(10)
                                        end if
                                    else
                                        statements = statements // "    " // "0" // char(10)
                                    end if
                                end if
                                deallocate(stmt_ast)
                            else
                                ! AST allocation failed - use direct reconstruction
                                if (size(stmt_tokens) > 0 .and. stmt_tokens(1)%kind == TK_KEYWORD .and. stmt_tokens(1)%text == "print") then
                                    stmt_code = reconstruct_line_from_tokens(stmt_tokens(1:size(stmt_tokens)-1))
                                    statements = statements // "    " // trim(stmt_code) // char(10)
                                else
                                    statements = statements // "    " // "0" // char(10)
                                end if
                            end if
                        end block
                        
                        deallocate(stmt_tokens)
                    end if
                end if
                
                ! Move past this statement and any following newlines
                i = stmt_end + 1
                do while (i <= size(current_tokens) .and. current_tokens(i)%kind == TK_NEWLINE)
                    i = i + 1
                end do
            end do
        end if
        
    end function generate_executable_statements_from_tokens

    ! FALLBACK: Generate function definitions from tokens until AST has function nodes  
    ! TODO: Remove when AST supports function definition nodes
    function generate_function_definitions_from_tokens() result(functions)
        character(len=:), allocatable :: functions
        integer :: i, stmt_start, stmt_end
        type(token_t), allocatable :: stmt_tokens(:)
        character(len=:), allocatable :: func_code, func_line
        
        functions = ""
        
        if (allocated(current_tokens)) then
            i = 1
            do while (i <= size(current_tokens))
                if (current_tokens(i)%kind == TK_EOF) exit
                
                ! Find statement boundaries
                stmt_start = i
                stmt_end = i
                do while (stmt_end <= size(current_tokens) .and. current_tokens(stmt_end)%kind /= TK_EOF)
                    stmt_end = stmt_end + 1
                end do
                stmt_end = stmt_end - 1
                
                if (stmt_end >= stmt_start) then
                    ! Check if this is a function definition
                    if (stmt_end >= stmt_start .and. is_function_def_statement(current_tokens(stmt_start:stmt_end))) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Generate complete function definition
                        func_line = reconstruct_line_from_tokens(stmt_tokens)
                        if (len_trim(func_line) > 0) then
                            ! Generate complete function with end statement
                            func_code = "    " // trim(func_line) // new_line('a')
                            func_code = func_code // "        ! TODO: Function body" // new_line('a')
                            func_code = func_code // "    end function" // new_line('a')
                            
                            functions = functions // func_code
                        end if
                        
                        deallocate(stmt_tokens)
                    end if
                end if
                
                i = stmt_end + 1
            end do
        end if
        
    end function generate_function_definitions_from_tokens

    ! Helper function to check if a line of tokens is a function definition
    function is_function_def_statement(tokens) result(is_func_def)
        type(token_t), intent(in) :: tokens(:)
        logical :: is_func_def
        
        is_func_def = .false.
        
        if (size(tokens) >= 2) then
            ! Check for "function" keyword
            if (tokens(1)%kind == TK_KEYWORD .and. tokens(1)%text == "function") then
                is_func_def = .true.
            ! Check for "type function" pattern
            else if (size(tokens) >= 3 .and. tokens(1)%kind == TK_IDENTIFIER .and. &
                     tokens(2)%kind == TK_KEYWORD .and. tokens(2)%text == "function") then
                is_func_def = .true.
            end if
        end if
    end function is_function_def_statement

    ! FALLBACK: Reconstruct line from tokens for unimplemented AST features
    ! TODO: Remove when all statements have proper AST nodes
    function reconstruct_line_from_tokens(tokens) result(line)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: line
        integer :: i
        
        line = ""
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_EOF) then
                if (i > 1) then
                    ! Add space before this token if needed  
                    if (tokens(i-1)%text == "," .and. i < size(tokens)) then
                        line = line // " "
                    else if (tokens(i-1)%text == ":") then
                        ! Always add space after colon
                        line = line // " "
                    else if (i > 1 .and. tokens(i)%kind /= TK_OPERATOR .and. tokens(i-1)%kind /= TK_OPERATOR) then
                        line = line // " "
                    end if
                end if
                line = line // trim(tokens(i)%text)
            end if
        end do
    end function reconstruct_line_from_tokens

    ! Get comma-separated list of function names from tokens  
    function get_function_names_from_tokens() result(func_names)
        character(len=:), allocatable :: func_names
        integer :: i, stmt_start, stmt_end
        type(token_t), allocatable :: stmt_tokens(:)
        character(len=:), allocatable :: func_name
        
        func_names = ""
        
        if (allocated(current_tokens)) then
            i = 1
            do while (i <= size(current_tokens))
                if (current_tokens(i)%kind == TK_EOF) exit
                
                ! Find statement boundaries
                stmt_start = i
                stmt_end = i
                do while (stmt_end <= size(current_tokens) .and. current_tokens(stmt_end)%kind /= TK_EOF)
                    stmt_end = stmt_end + 1
                end do
                stmt_end = stmt_end - 1
                
                if (stmt_end >= stmt_start) then
                    ! Check if this is a function definition
                    if (stmt_end >= stmt_start .and. is_function_def_statement(current_tokens(stmt_start:stmt_end))) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Extract function name
                        func_name = extract_function_name_from_tokens(stmt_tokens)
                        if (len_trim(func_name) > 0) then
                            if (len_trim(func_names) > 0) then
                                func_names = func_names // "," // trim(func_name)
                            else
                                func_names = trim(func_name)
                            end if
                        end if
                        
                        deallocate(stmt_tokens)
                    end if
                end if
                
                i = stmt_end + 1
            end do
        end if
        
    end function get_function_names_from_tokens

    ! Extract function name from function definition tokens
    function extract_function_name_from_tokens(tokens) result(func_name)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: func_name
        integer :: i
        logical :: found_function_keyword
        
        func_name = ""
        found_function_keyword = .false.
        
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "function") then
                found_function_keyword = .true.
            else if (found_function_keyword .and. tokens(i)%kind == TK_IDENTIFIER) then
                func_name = trim(tokens(i)%text)
                return
            end if
        end do
    end function extract_function_name_from_tokens

end module token_fallback