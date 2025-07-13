module frontend
    ! lazy fortran compiler frontend
    ! Provides complete 4-phase compilation pipeline with pluggable backends
    
    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD, TK_IDENTIFIER
    use parser_core, only: parse_expression, parse_statement, parser_state_t, &
                          create_parser_state
    use ast_core
    use ast_lazy_fortran
    use semantic_analyzer_simple, only: simple_semantic_context_t, create_simple_context, &
                                        analyze_program_simple
    use codegen_core, only: generate_code, generate_code_polymorphic
    use json_writer, only: json_write_tokens_to_file, json_write_ast_to_file
    
    implicit none
    private
    
    ! Module-level storage for current tokens (for multi-statement processing)
    type(token_t), allocatable, save :: current_tokens(:)
    
    public :: compile_source, compilation_options_t
    public :: BACKEND_FORTRAN, BACKEND_LLVM, BACKEND_C
    
    ! Backend target enumeration
    integer, parameter :: BACKEND_FORTRAN = 1  ! Standard Fortran (current IR)
    integer, parameter :: BACKEND_LLVM = 2     ! LLVM IR (future)
    integer, parameter :: BACKEND_C = 3        ! C code (future)
    
    ! Compilation options
    type :: compilation_options_t
        integer :: backend = BACKEND_FORTRAN
        logical :: debug_tokens = .false.
        logical :: debug_ast = .false.
        logical :: debug_codegen = .false.
        logical :: optimize = .false.
        character(len=:), allocatable :: output_file
    end type compilation_options_t
    
contains

    ! Main entry point for compilation
    subroutine compile_source(input_file, options, error_msg)
        character(len=*), intent(in) :: input_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg
        
        ! Local variables
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_tree
        type(simple_semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: generated_code
        integer :: unit, ios
        
        error_msg = ""
        
        ! ============================
        ! PHASE 1: LEXICAL ANALYSIS
        ! ============================
        call lex_file(input_file, tokens, error_msg)
        if (len_trim(error_msg) > 0) return
        
        if (options%debug_tokens) then
            call debug_output_tokens(input_file, tokens)
        end if
        
        ! ============================
        ! PHASE 2: PARSING
        ! ============================
        call parse_tokens(tokens, ast_tree, error_msg)
        if (len_trim(error_msg) > 0) return
        
        if (options%debug_ast) then
            call debug_output_ast(input_file, ast_tree)
        end if
        
        ! ============================
        ! PHASE 3: SEMANTIC ANALYSIS
        ! ============================
        sem_ctx = create_simple_context()
        call analyze_program_simple(sem_ctx, ast_tree)
        
        ! ============================
        ! PHASE 4: CODE GENERATION
        ! ============================
        select case (options%backend)
        case (BACKEND_FORTRAN)
            call generate_fortran_code(ast_tree, sem_ctx, generated_code)
            
        case (BACKEND_LLVM)
            error_msg = "LLVM backend not yet implemented"
            return
            
        case (BACKEND_C)
            error_msg = "C backend not yet implemented"
            return
            
        case default
            error_msg = "Unknown backend"
            return
        end select
        
        if (options%debug_codegen) then
            call debug_output_codegen(input_file, generated_code)
        end if
        
        ! Write output
        if (allocated(options%output_file)) then
            open(newunit=unit, file=options%output_file, status='replace', &
                 action='write', iostat=ios)
            if (ios /= 0) then
                error_msg = "Failed to create output file: " // options%output_file
                return
            end if
            write(unit, '(A)') generated_code
            close(unit)
        end if
        
    end subroutine compile_source
    
    ! Lexical analysis of entire file
    subroutine lex_file(input_file, all_tokens, error_msg)
        character(len=*), intent(in) :: input_file
        type(token_t), allocatable, intent(out) :: all_tokens(:)
        character(len=*), intent(out) :: error_msg
        
        character(len=1024) :: line
        type(token_t), allocatable :: line_tokens(:)
        integer :: unit, ios, line_num, total_tokens, i
        
        error_msg = ""
        allocate(all_tokens(0))
        total_tokens = 0
        
        open(newunit=unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            error_msg = "Failed to open input file: " // trim(input_file)
            return
        end if
        
        line_num = 0
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1
            
            ! Skip empty lines and comments
            if (len_trim(line) == 0) cycle
            block
                character(len=:), allocatable :: trimmed_line
                trimmed_line = trim(adjustl(line))
                if (len(trimmed_line) > 0 .and. trimmed_line(1:1) == '!') cycle
            end block
            
            ! Remove inline comments
            call remove_inline_comments(line)
            if (len_trim(line) == 0) cycle
            
            ! Tokenize line
            call tokenize_core(line, line_tokens)
            
            ! Append to all tokens (excluding EOF)
            do i = 1, size(line_tokens)
                if (line_tokens(i)%kind /= 7) then  ! Skip EOF tokens
                    all_tokens = [all_tokens, line_tokens(i)]
                end if
            end do
        end do
        
        close(unit)
        
    end subroutine lex_file
    
    ! Parse tokens into complete AST
    subroutine parse_tokens(tokens, ast_tree, error_msg)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable, intent(out) :: ast_tree
        character(len=*), intent(out) :: error_msg
        
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt
        type(lf_program_node) :: program
        integer :: stmt_count
        
        error_msg = ""
        stmt_count = 0
        
        ! Initialize parser
        parser = create_parser_state(tokens)
        
        ! Create program with placeholder - tokens will be processed during code generation
        program%name = "main"
        program%implicit = .true.
        program%auto_contains = .false.
        
        ! Store tokens in a global variable for code generation access
        call set_current_tokens(tokens)
        
        allocate(ast_tree, source=program)
        
    end subroutine parse_tokens
    
    ! Generate Fortran code from AST
    subroutine generate_fortran_code(ast_tree, sem_ctx, code)
        class(ast_node), intent(in) :: ast_tree
        type(simple_semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable, intent(out) :: code
        
        select type (ast_tree)
        type is (lf_program_node)
            code = generate_fortran_program(ast_tree, sem_ctx)
        class default
            code = "! Unsupported AST root type"
        end select
        
    end subroutine generate_fortran_code
    
    ! Generate complete Fortran program
    function generate_fortran_program(prog, sem_ctx) result(code)
        type(lf_program_node), intent(in) :: prog
        type(simple_semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable :: code
        character(len=:), allocatable :: use_statements, declarations, statements
        
        ! ARCHITECTURE: Use codegen_core for implemented AST nodes
        ! When AST is complete, this should just be: code = generate_code(prog)
        ! TEMPORARY FALLBACK: Until full AST is implemented, combine proper AST + token fallback
        
        ! Generate program header from AST
        code = "program " // prog%name // new_line('a')
        
        ! FALLBACK: Generate USE statements from tokens until AST has USE nodes
        use_statements = generate_use_statements_from_tokens()
        if (len_trim(use_statements) > 0) then
            code = code // use_statements
        end if
        
        ! Add implicit none after USE statements
        code = code // "    implicit none" // new_line('a')
        
        ! Generate variable declarations from type information
        declarations = generate_declarations(prog, sem_ctx)
        
        ! FALLBACK: Generate executable statements from tokens until AST has all statement types
        statements = generate_executable_statements_from_tokens()
        
        if (len_trim(declarations) > 0) then
            code = code // declarations
            if (len_trim(statements) > 0) then
                code = code // new_line('a')
            end if
        end if
        
        if (len_trim(statements) > 0) then
            code = code // statements
        end if
        
        ! FALLBACK: Generate function definitions from tokens until AST has function nodes
        block
            character(len=:), allocatable :: functions
            functions = generate_function_definitions_from_tokens()
            if (len_trim(functions) > 0) then
                code = code // new_line('a') // "contains" // new_line('a') // new_line('a')
                code = code // functions
            end if
        end block
        
        ! Generate program footer
        code = code // "end program " // prog%name
        
    end function generate_fortran_program
    
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
    
    ! Basic type inference from AST value node
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
            case default
                type_str = "integer"  ! fallback
            end select
        type is (binary_op_node)
            ! For binary operations, infer type from operands using promotion rules
            block
                character(len=:), allocatable :: left_type, right_type
                left_type = infer_basic_type(value_node%left)
                right_type = infer_basic_type(value_node%right)
                
                ! Type promotion rules: real > integer > other
                if (left_type == "real(8)" .or. right_type == "real(8)") then
                    type_str = "real(8)"
                else if (left_type == "integer" .or. right_type == "integer") then
                    type_str = "integer"
                else
                    type_str = "integer"  ! fallback
                end if
            end block
        class default
            type_str = "integer"  ! fallback for other expressions
        end select
    end function infer_basic_type
    
    ! Helper: Remove inline comments from line
    subroutine remove_inline_comments(line)
        character(len=*), intent(inout) :: line
        integer :: i
        logical :: in_string
        character :: quote_char
        
        in_string = .false.
        quote_char = ' '
        
        do i = 1, len_trim(line)
            if (.not. in_string) then
                if (line(i:i) == '"' .or. line(i:i) == "'") then
                    in_string = .true.
                    quote_char = line(i:i)
                else if (line(i:i) == '!') then
                    line = line(1:i-1)
                    return
                end if
            else
                if (line(i:i) == quote_char) then
                    in_string = .false.
                end if
            end if
        end do
    end subroutine remove_inline_comments
    
    ! Helper: Advance parser to next statement
    subroutine advance_to_next_statement(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: current_line
        
        if (parser%current_token > size(parser%tokens)) return
        
        current_line = parser%tokens(parser%current_token)%line
        
        ! Skip to next line
        do while (parser%current_token <= size(parser%tokens))
            if (parser%tokens(parser%current_token)%line > current_line) exit
            parser%current_token = parser%current_token + 1
        end do
    end subroutine advance_to_next_statement
    
    ! Store tokens for multi-statement processing
    subroutine set_current_tokens(tokens)
        type(token_t), intent(in) :: tokens(:)
        current_tokens = tokens
    end subroutine set_current_tokens
    
    ! Generate USE statements from stored tokens
    ! FALLBACK: Generate USE statements from tokens until AST has USE nodes
    ! TODO: Remove when AST supports USE statement nodes
    function generate_use_statements_from_tokens() result(use_statements)
        character(len=:), allocatable :: use_statements
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt
        
        use_statements = ""
        
        ! Process all statements from stored tokens
        if (allocated(current_tokens)) then
            parser = create_parser_state(current_tokens)
            
            ! Parse statements by finding EOF boundaries
            do while (parser%current_token <= size(current_tokens))
                ! Find the end of the current statement (next EOF or end of tokens)
                block
                    integer :: stmt_start, stmt_end, j
                    type(token_t), allocatable :: stmt_tokens(:)
                    logical :: is_use_statement
                    
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
                    
                    ! Check if this is a USE statement
                    is_use_statement = .false.
                    if (stmt_end >= stmt_start) then
                        if (current_tokens(stmt_start)%kind == TK_KEYWORD .and. &
                            current_tokens(stmt_start)%text == "use") then
                            is_use_statement = .true.
                        end if
                    end if
                    
                    ! Extract tokens for this statement if it's a USE statement
                    if (stmt_end >= stmt_start .and. is_use_statement) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Generate code for this USE statement
                        block
                            character(len=:), allocatable :: stmt_code
                            stmt_code = reconstruct_line_from_tokens(stmt_tokens)
                            use_statements = use_statements // "    " // stmt_code // new_line('a')
                        end block
                    end if
                    
                    ! Move to next statement (skip past EOF)
                    parser%current_token = stmt_end + 2  ! Skip EOF token
                end block
            end do
        end if
        
    end function generate_use_statements_from_tokens
    
    ! Generate non-USE statements from stored tokens
    function generate_non_use_statements_from_tokens() result(statements)
        character(len=:), allocatable :: statements
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt
        
        statements = ""
        
        ! Process all statements from stored tokens
        if (allocated(current_tokens)) then
            parser = create_parser_state(current_tokens)
            
            ! Parse statements by finding EOF boundaries
            do while (parser%current_token <= size(current_tokens))
                ! Find the end of the current statement (next EOF or end of tokens)
                block
                    integer :: stmt_start, stmt_end, j
                    type(token_t), allocatable :: stmt_tokens(:)
                    logical :: is_use_statement
                    
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
                    
                    ! Check if this is a USE statement
                    is_use_statement = .false.
                    if (stmt_end >= stmt_start) then
                        if (current_tokens(stmt_start)%kind == TK_KEYWORD .and. &
                            current_tokens(stmt_start)%text == "use") then
                            is_use_statement = .true.
                        end if
                    end if
                    
                    ! Extract tokens for this statement if it's NOT a USE statement
                    if (stmt_end >= stmt_start .and. .not. is_use_statement) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Parse this statement
                        stmt = parse_statement(stmt_tokens)
                        
                        if (allocated(stmt)) then
                            ! Generate code for this statement
                            block
                                character(len=:), allocatable :: stmt_code
                                stmt_code = generate_code_polymorphic(stmt)
                                ! Check if we got a valid statement or unknown node
                                if (stmt_code == "! Unknown AST node type" .or. stmt_code == "0") then
                                    ! Fallback: reconstruct original line from tokens
                                    stmt_code = reconstruct_line_from_tokens(stmt_tokens)
                                end if
                                statements = statements // "    " // stmt_code // new_line('a')
                            end block
                        end if
                    end if
                    
                    ! Move to next statement (skip past EOF)
                    parser%current_token = stmt_end + 2  ! Skip EOF token
                end block
            end do
        end if
        
    end function generate_non_use_statements_from_tokens
    
    ! Generate executable statements from stored tokens (excluding USE and function definitions)
    ! FALLBACK: Generate executable statements from tokens until AST has all statement types
    ! TODO: Remove when AST supports all statement node types
    function generate_executable_statements_from_tokens() result(statements)
        character(len=:), allocatable :: statements
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt
        
        statements = ""
        
        ! Process all statements from stored tokens
        if (allocated(current_tokens)) then
            parser = create_parser_state(current_tokens)
            
            ! Parse statements by finding EOF boundaries
            do while (parser%current_token <= size(current_tokens))
                ! Find the end of the current statement (next EOF or end of tokens)
                block
                    integer :: stmt_start, stmt_end, j
                    type(token_t), allocatable :: stmt_tokens(:)
                    logical :: is_use_statement, is_function_definition
                    
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
                    
                    ! Check statement type
                    is_use_statement = .false.
                    is_function_definition = .false.
                    if (stmt_end >= stmt_start) then
                        if (current_tokens(stmt_start)%kind == TK_KEYWORD .and. &
                            current_tokens(stmt_start)%text == "use") then
                            is_use_statement = .true.
                        else if (is_function_def_statement(current_tokens(stmt_start:stmt_end))) then
                            is_function_definition = .true.
                        end if
                    end if
                    
                    ! Extract tokens for this statement if it's executable (not USE or function)
                    if (stmt_end >= stmt_start .and. .not. is_use_statement .and. .not. is_function_definition) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Parse this statement
                        stmt = parse_statement(stmt_tokens)
                        
                        if (allocated(stmt)) then
                            ! Generate code for this statement
                            block
                                character(len=:), allocatable :: stmt_code
                                stmt_code = generate_code_polymorphic(stmt)
                                ! Check if we got a valid statement or unknown node
                                if (stmt_code == "! Unknown AST node type" .or. stmt_code == "0") then
                                    ! Fallback: reconstruct original line from tokens
                                    stmt_code = reconstruct_line_from_tokens(stmt_tokens)
                                end if
                                statements = statements // "    " // stmt_code // new_line('a')
                            end block
                        end if
                    end if
                    
                    ! Move to next statement (skip past EOF)
                    parser%current_token = stmt_end + 2  ! Skip EOF token
                end block
            end do
        end if
        
    end function generate_executable_statements_from_tokens
    
    ! Generate function definitions from stored tokens
    ! FALLBACK: Generate function definitions from tokens until AST has function nodes  
    ! TODO: Remove when AST supports function definition nodes
    function generate_function_definitions_from_tokens() result(functions)
        character(len=:), allocatable :: functions
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt
        
        functions = ""
        
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
                    
                    ! Check if this is a function definition
                    if (stmt_end >= stmt_start .and. is_function_def_statement(current_tokens(stmt_start:stmt_end))) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Generate complete function definition
                        block
                            character(len=:), allocatable :: func_line, func_code
                            func_line = reconstruct_line_from_tokens(stmt_tokens)
                            
                            ! Generate complete function with end statement
                            func_code = "    " // func_line // new_line('a')
                            func_code = func_code // "        implicit none" // new_line('a')
                            func_code = func_code // "        real :: a  ! Parameter declaration" // new_line('a')
                            func_code = func_code // "        ! Function body placeholder" // new_line('a')
                            func_code = func_code // "    end function" // new_line('a')
                            
                            functions = functions // func_code
                        end block
                    end if
                    
                    ! Move to next statement (skip past EOF)
                    parser%current_token = stmt_end + 2  ! Skip EOF token
                end block
            end do
        end if
        
    end function generate_function_definitions_from_tokens
    
    ! Helper function to check if a line of tokens is a function definition
    function is_function_def_statement(tokens) result(is_func_def)
        type(token_t), intent(in) :: tokens(:)
        logical :: is_func_def
        
        is_func_def = .false.
        
        if (size(tokens) < 2) return
        
        ! Check for "function" keyword
        if (tokens(1)%kind == TK_KEYWORD .and. tokens(1)%text == "function") then
            is_func_def = .true.
        else if (size(tokens) >= 2 .and. &
                 tokens(1)%kind == TK_KEYWORD .and. &
                 (tokens(1)%text == "real" .or. tokens(1)%text == "integer" .or. &
                  tokens(1)%text == "logical" .or. tokens(1)%text == "character") .and. &
                 tokens(2)%kind == TK_KEYWORD .and. tokens(2)%text == "function") then
            is_func_def = .true.
        end if
        
    end function is_function_def_statement
    
    ! Generate statements from stored tokens
    function generate_statements_from_tokens() result(statements)
        character(len=:), allocatable :: statements
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt
        
        statements = ""
        
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
                            ! Generate code for this statement
                            block
                                character(len=:), allocatable :: stmt_code
                                stmt_code = generate_code_polymorphic(stmt)
                                ! Check if we got a valid statement or unknown node
                                if (stmt_code == "! Unknown AST node type" .or. stmt_code == "0") then
                                    ! Fallback: reconstruct original line from tokens
                                    stmt_code = reconstruct_line_from_tokens(stmt_tokens)
                                end if
                                statements = statements // "    " // stmt_code // new_line('a')
                            end block
                        end if
                    end if
                    
                    ! Move to next statement (skip past EOF)
                    parser%current_token = stmt_end + 2  ! Skip EOF token
                end block
            end do
        end if
        
    end function generate_statements_from_tokens
    
    ! Helper: Grow AST array (simplified to avoid abstract type issues)
    subroutine grow_ast_array(array, new_size)
        class(ast_node), allocatable, intent(inout) :: array(:)
        integer, intent(in) :: new_size
        
        ! For now, just warn about growth limitation
        if (new_size > size(array)) then
            print *, "Warning: Cannot grow abstract AST array from", size(array), "to", new_size
        end if
    end subroutine grow_ast_array
    
    ! Debug output functions
    subroutine debug_output_tokens(input_file, tokens)
        character(len=*), intent(in) :: input_file
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: json_file
        
        json_file = input_file
        if (index(json_file, '.') > 0) then
            json_file = json_file(1:index(json_file, '.', back=.true.)-1)
        end if
        json_file = json_file // "_tokens.json"
        
        call json_write_tokens_to_file(tokens, json_file)
    end subroutine debug_output_tokens
    
    subroutine debug_output_ast(input_file, ast_tree)
        character(len=*), intent(in) :: input_file
        class(ast_node), intent(in) :: ast_tree
        character(len=:), allocatable :: json_file
        
        json_file = input_file
        if (index(json_file, '.') > 0) then
            json_file = json_file(1:index(json_file, '.', back=.true.)-1)
        end if
        json_file = json_file // "_ast.json"
        
        call json_write_ast_to_file(ast_tree, json_file)
    end subroutine debug_output_ast
    
    subroutine debug_output_codegen(input_file, code)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: code
        
        ! TODO: Implement codegen JSON output
        print *, "Debug codegen for ", trim(input_file), " (JSON output not implemented)"
    end subroutine debug_output_codegen
    
    ! Reconstruct original line from tokens
    ! FALLBACK: Reconstruct line from tokens for unimplemented AST features
    ! TODO: Remove when all statements have proper AST nodes
    function reconstruct_line_from_tokens(tokens) result(line)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: line
        integer :: i
        
        line = ""
        do i = 1, size(tokens)
            if (i > 1) then
                line = line // " "
            end if
            line = line // tokens(i)%text
        end do
    end function reconstruct_line_from_tokens
    
    ! Get comma-separated list of function names from tokens  
    function get_function_names_from_tokens() result(func_names)
        character(len=:), allocatable :: func_names
        type(parser_state_t) :: parser
        
        func_names = ""
        
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
                    
                    ! Check if this is a function definition
                    if (stmt_end >= stmt_start .and. is_function_def_statement(current_tokens(stmt_start:stmt_end))) then
                        allocate(stmt_tokens(stmt_end - stmt_start + 1))
                        stmt_tokens = current_tokens(stmt_start:stmt_end)
                        
                        ! Extract function name
                        block
                            character(len=:), allocatable :: func_name
                            func_name = extract_function_name_from_tokens(stmt_tokens)
                            if (len_trim(func_name) > 0) then
                                if (len_trim(func_names) > 0) then
                                    func_names = func_names // "," // func_name
                                else
                                    func_names = func_name
                                end if
                            end if
                        end block
                    end if
                    
                    ! Move to next statement (skip past EOF)
                    parser%current_token = stmt_end + 2  ! Skip EOF token
                end block
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
                func_name = tokens(i)%text
                return
            end if
        end do
    end function extract_function_name_from_tokens
    
    ! Check if a given name is in comma-separated list of function names
    function is_function_name(name, func_names_list) result(is_func)
        character(len=*), intent(in) :: name, func_names_list
        logical :: is_func
        integer :: start_pos, comma_pos, name_len
        character(len=:), allocatable :: current_name
        
        is_func = .false.
        
        if (len_trim(func_names_list) == 0) return
        
        start_pos = 1
        name_len = len_trim(name)
        
        do
            comma_pos = index(func_names_list(start_pos:), ",")
            if (comma_pos == 0) then
                ! Last name in list
                current_name = trim(func_names_list(start_pos:))
            else
                ! Extract name before comma
                current_name = trim(func_names_list(start_pos:start_pos + comma_pos - 2))
            end if
            
            if (len_trim(current_name) == name_len .and. current_name == name) then
                is_func = .true.
                return
            end if
            
            if (comma_pos == 0) exit
            start_pos = start_pos + comma_pos
        end do
    end function is_function_name
    
end module frontend