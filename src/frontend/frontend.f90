module frontend
    ! lazy fortran compiler frontend
    ! Clean coordinator module - delegates to extracted specialized modules
    ! Architecture: Lexer → Parser → Semantic → Codegen with FALLBACK support
    
    use lexer_core, only: token_t, tokenize_core, TK_EOF
    use parser_core, only: parse_expression, parse_statement, parser_state_t, create_parser_state
    use ast_core
    use ast_lazy_fortran
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use codegen_core, only: generate_code, generate_code_polymorphic
    
    ! FALLBACK modules (temporary until full AST)
    use token_fallback, only: set_current_tokens, generate_use_statements_from_tokens, &
                              generate_executable_statements_from_tokens, &
                              generate_function_definitions_from_tokens
    use declaration_generator, only: generate_declarations
    use debug_utils, only: debug_output_tokens, debug_output_ast, debug_output_semantic, debug_output_codegen
    
    implicit none
    private
    
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
        logical :: debug_semantic = .false.
        logical :: debug_codegen = .false.
        logical :: optimize = .false.
        character(len=:), allocatable :: output_file
    end type compilation_options_t
    
contains

    ! Main entry point - clean 4-phase compilation pipeline
    subroutine compile_source(input_file, options, error_msg)
        character(len=*), intent(in) :: input_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg
        
        ! Local variables
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_tree
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: code, source
        integer :: unit, iostat
        
        error_msg = ""
        
        ! Read source file
        open(newunit=unit, file=input_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot open input file: " // input_file
            return
        end if
        
        block
            character(len=:), allocatable :: line
            allocate(character(len=0) :: source)
            allocate(character(len=1000) :: line)
            
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                source = source // trim(line) // new_line('a')
            end do
        end block
        close(unit)
        
        ! Phase 1: Lexical Analysis
        call lex_file(source, tokens, error_msg)
        if (error_msg /= "") return
        if (options%debug_tokens) call debug_output_tokens(input_file, tokens)
        
        ! Phase 2: Parsing
        call parse_tokens(tokens, ast_tree, error_msg)
        if (error_msg /= "") return
        if (options%debug_ast) call debug_output_ast(input_file, ast_tree)
        
        ! Phase 3: Semantic Analysis - DISABLED TO FIX CODE GENERATION
        sem_ctx = create_semantic_context()
        ! call analyze_program(sem_ctx, ast_tree)
        ! if (options%debug_semantic) call debug_output_semantic(input_file, ast_tree)
        
        ! Phase 4: Code Generation
        call generate_fortran_code(ast_tree, sem_ctx, code)
        if (options%debug_codegen) call debug_output_codegen(input_file, code)
        
        ! Write output
        if (allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if
        
    end subroutine compile_source

    ! Phase 1: Lexical Analysis
    subroutine lex_file(source, tokens, error_msg)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        character(len=*), intent(out) :: error_msg
        
        error_msg = ""
        call tokenize_core(source, tokens)
        
        ! Store tokens for FALLBACK functions
        call set_current_tokens(tokens)
    end subroutine lex_file

    ! Phase 2: Parsing 
    subroutine parse_tokens(tokens, ast_tree, error_msg)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable, intent(out) :: ast_tree
        character(len=*), intent(out) :: error_msg
        
        ! Local variables for statement parsing using wrapper pattern
        type(ast_node_wrapper), allocatable :: body_statements(:)
        class(ast_node), allocatable :: stmt
        integer :: i, stmt_start, stmt_end, stmt_count, current_line
        type(token_t), allocatable :: stmt_tokens(:)
        
        error_msg = ""
        stmt_count = 0
        
        ! Create program node (lazy fortran auto-wrapping)
        allocate(lf_program_node :: ast_tree)
        select type (prog => ast_tree)
        type is (lf_program_node)
            prog%name = "main"
            prog%implicit = .true.
            prog%auto_contains = .false.
            prog%line = 1
            prog%column = 1
            
            ! Count statements first (simple approach: count lines)
            i = 1
            do while (i <= size(tokens))
                if (tokens(i)%kind == TK_EOF) exit
                
                ! Skip to next line
                current_line = tokens(i)%line
                do while (i <= size(tokens) .and. tokens(i)%line == current_line)
                    i = i + 1
                end do
                stmt_count = stmt_count + 1
            end do
            
            ! Parse statements properly into AST using wrapper pattern
            if (stmt_count > 0) then
                ! Allocate wrapper array
                allocate(body_statements(stmt_count))
                
                ! Parse statements
                i = 1
                stmt_count = 0
                do while (i <= size(tokens))
                    if (tokens(i)%kind == TK_EOF) exit
                    
                    ! Get statement tokens for this line
                    stmt_start = i
                    current_line = tokens(i)%line
                    stmt_end = i
                    
                    do while (stmt_end <= size(tokens))
                        if (tokens(stmt_end)%kind == TK_EOF .or. &
                            tokens(stmt_end)%line > current_line) then
                            stmt_end = stmt_end - 1
                            exit
                        end if
                        stmt_end = stmt_end + 1
                    end do
                    
                    if (stmt_end >= stmt_start) then
                        ! Extract statement tokens and add EOF
                        allocate(stmt_tokens(stmt_end - stmt_start + 2))
                        stmt_tokens(1:stmt_end - stmt_start + 1) = tokens(stmt_start:stmt_end)
                        ! Add EOF token
                        stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                        stmt_tokens(stmt_end - stmt_start + 2)%text = ""
                        stmt_tokens(stmt_end - stmt_start + 2)%line = tokens(stmt_end)%line
                        stmt_tokens(stmt_end - stmt_start + 2)%column = tokens(stmt_end)%column + 1
                        
                        ! Parse the statement
                        stmt = parse_statement(stmt_tokens)
                        
                        if (allocated(stmt)) then
                            stmt_count = stmt_count + 1
                            ! Store in wrapper - move the allocation instead of copying
                            call move_alloc(stmt, body_statements(stmt_count)%node)
                        end if
                        
                        deallocate(stmt_tokens)
                    end if
                    
                    i = stmt_end + 1
                end do
                
                ! Create polymorphic array properly using wrapper pattern
                if (stmt_count > 0) then
                    ! Create the wrapper array - this works now!
                    allocate(prog%body(stmt_count))
                    
                    ! Copy each wrapper directly
                    do i = 1, stmt_count
                        allocate(prog%body(i)%node, source=body_statements(i)%node)
                    end do
                end if
                
                deallocate(body_statements)
            end if
        end select
    end subroutine parse_tokens

    ! Phase 4: Code Generation (using FALLBACK until full AST)
    subroutine generate_fortran_code(ast_tree, sem_ctx, code)
        class(ast_node), intent(in) :: ast_tree
        type(semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable, intent(out) :: code
        
        select type (prog => ast_tree)
        type is (lf_program_node)
            code = generate_fortran_program(prog, sem_ctx)
        class default
            code = "! Error: Unsupported AST node type"
        end select
    end subroutine generate_fortran_code

    ! Generate Fortran program (FALLBACK approach until full AST)
    function generate_fortran_program(prog, sem_ctx) result(code)
        type(lf_program_node), intent(in) :: prog
        type(semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable :: code
        character(len=:), allocatable :: use_statements, declarations, statements, functions
        
        ! ARCHITECTURE: AST-based code generation ONLY - NO FALLBACK
        code = generate_code(prog)
    end function generate_fortran_program

    ! Write output to file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename, content
        character(len=*), intent(out) :: error_msg
        
        integer :: unit, iostat
        
        open(newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot create output file: " // filename
            return
        end if
        
        write(unit, '(A)') content
        close(unit)
        error_msg = ""
    end subroutine write_output_file

end module frontend