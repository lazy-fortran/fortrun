module frontend
    ! lazy fortran compiler frontend
    ! Clean coordinator module - delegates to extracted specialized modules
    ! Architecture: Lexer → Parser → Semantic → Codegen with FALLBACK support
    
    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD
    use parser_core, only: parse_expression, parse_statement, parser_state_t, create_parser_state, parse_function_definition
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
        
        ! DEBUG: Print entry point
        print *, "DEBUG: compile_source called with: ", trim(input_file)
        
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
        
        ! Phase 3: Semantic Analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, ast_tree)
        if (options%debug_semantic) call debug_output_semantic(input_file, ast_tree)
        
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
        
        ! Local variables for program unit parsing using wrapper pattern
        type(ast_node_wrapper), allocatable :: body_statements(:)
        class(ast_node), allocatable :: stmt
        integer :: i, unit_start, unit_end, stmt_count
        type(token_t), allocatable :: unit_tokens(:)
        
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
            
            ! Parse program units, not individual lines
            i = 1
            do while (i <= size(tokens))
                if (tokens(i)%kind == TK_EOF) exit
                
                ! Find program unit boundary
                call find_program_unit_boundary(tokens, i, unit_start, unit_end)
                
                if (unit_end >= unit_start) then
                    ! Extract unit tokens and add EOF
                    allocate(unit_tokens(unit_end - unit_start + 2))
                    unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
                    ! Add EOF token
                    unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
                    unit_tokens(unit_end - unit_start + 2)%text = ""
                    unit_tokens(unit_end - unit_start + 2)%line = tokens(unit_end)%line
                    unit_tokens(unit_end - unit_start + 2)%column = tokens(unit_end)%column + 1
                    
                    ! Parse the program unit
                    stmt = parse_program_unit(unit_tokens)
                    
                    if (allocated(stmt)) then
                        ! Extend wrapper array using [array, new_element] pattern
                        block
                            type(ast_node_wrapper) :: new_wrapper
                            allocate(new_wrapper%node, source=stmt)
                            if (allocated(body_statements)) then
                                body_statements = [body_statements, new_wrapper]
                            else
                                body_statements = [new_wrapper]
                            end if
                            stmt_count = stmt_count + 1
                        end block
                    end if
                    
                    deallocate(unit_tokens)
                end if
                
                i = unit_end + 1
            end do
            
            ! Create polymorphic array properly using wrapper pattern
            if (stmt_count > 0) then
                ! Create the wrapper array - this works now!
                allocate(prog%body(stmt_count))
                
                ! Copy each wrapper directly
                do i = 1, stmt_count
                    allocate(prog%body(i)%node, source=body_statements(i)%node)
                end do
                
                deallocate(body_statements)
            end if
        end select
    end subroutine parse_tokens

    ! Find program unit boundary (function/subroutine/module spans multiple lines)
    subroutine find_program_unit_boundary(tokens, start_pos, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: unit_start, unit_end
        
        integer :: i, current_line, nesting_level
        logical :: in_function, in_subroutine, in_module
        
        unit_start = start_pos
        unit_end = start_pos
        in_function = .false.
        in_subroutine = .false.
        in_module = .false.
        nesting_level = 0
        
        ! Check if starting token indicates a multi-line construct
        if (start_pos <= size(tokens)) then
            ! Look for function definition patterns
            if (is_function_start(tokens, start_pos)) then
                in_function = .true.
                nesting_level = 1
            else if (is_subroutine_start(tokens, start_pos)) then
                in_subroutine = .true.
                nesting_level = 1
            else if (is_module_start(tokens, start_pos)) then
                in_module = .true.
                nesting_level = 1
            end if
        end if
        
        ! If this is a multi-line construct, find the end
        if (in_function .or. in_subroutine .or. in_module) then
            i = start_pos
            do while (i <= size(tokens) .and. nesting_level > 0)
                if (tokens(i)%kind == TK_EOF) exit
                
                ! Check for nested constructs
                if (in_function .and. is_function_start(tokens, i)) then
                    nesting_level = nesting_level + 1
                else if (in_subroutine .and. is_subroutine_start(tokens, i)) then
                    nesting_level = nesting_level + 1
                else if (in_module .and. is_module_start(tokens, i)) then
                    nesting_level = nesting_level + 1
                end if
                
                ! Check for end constructs
                if (in_function .and. is_end_function(tokens, i)) then
                    nesting_level = nesting_level - 1
                else if (in_subroutine .and. is_end_subroutine(tokens, i)) then
                    nesting_level = nesting_level - 1
                else if (in_module .and. is_end_module(tokens, i)) then
                    nesting_level = nesting_level - 1
                end if
                
                unit_end = i
                i = i + 1
            end do
        else
            ! Single line construct - find end of current line
            current_line = tokens(start_pos)%line
            i = start_pos
            do while (i <= size(tokens) .and. tokens(i)%line == current_line)
                unit_end = i
                i = i + 1
            end do
        end if
    end subroutine find_program_unit_boundary

    ! Parse a program unit (function, subroutine, module, or statement)
    function parse_program_unit(tokens) result(unit)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: unit
        type(parser_state_t) :: parser
        
        ! Check what type of program unit this is
        if (is_function_start(tokens, 1)) then
            ! Multi-line function definition - use proper parser
            parser = create_parser_state(tokens)
            unit = parse_function_definition(parser)
        else if (is_subroutine_start(tokens, 1)) then
            ! Multi-line subroutine definition - fallback to statement parser for now
            unit = parse_statement(tokens)
        else if (is_module_start(tokens, 1)) then
            ! Multi-line module definition - fallback to statement parser for now
            unit = parse_statement(tokens)
        else
            ! Single statement
            unit = parse_statement(tokens)
        end if
    end function parse_program_unit

    ! Helper functions to detect program unit types
    logical function is_function_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_function_start = .false.
        if (pos > size(tokens)) return
        
        ! Check for "function" keyword
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "function") then
            is_function_start = .true.
        ! Check for "type function" pattern
        else if (tokens(pos)%kind == TK_KEYWORD .and. &
                 (tokens(pos)%text == "real" .or. tokens(pos)%text == "integer" .or. &
                  tokens(pos)%text == "logical" .or. tokens(pos)%text == "character")) then
            if (pos + 1 <= size(tokens) .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. &
                tokens(pos + 1)%text == "function") then
                is_function_start = .true.
            end if
        end if
    end function is_function_start

    logical function is_subroutine_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_subroutine_start = .false.
        if (pos > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "subroutine") then
            is_subroutine_start = .true.
        end if
    end function is_subroutine_start

    logical function is_module_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_module_start = .false.
        if (pos > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "module") then
            is_module_start = .true.
        end if
    end function is_module_start

    logical function is_end_function(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_function = .false.
        if (pos + 1 > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
            tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "function") then
            is_end_function = .true.
        end if
    end function is_end_function

    logical function is_end_subroutine(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_subroutine = .false.
        if (pos + 1 > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
            tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "subroutine") then
            is_end_subroutine = .true.
        end if
    end function is_end_subroutine

    logical function is_end_module(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_module = .false.
        if (pos + 1 > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
            tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "module") then
            is_end_module = .true.
        end if
    end function is_end_module

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