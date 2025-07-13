module frontend
    ! lazy fortran compiler frontend
    ! Provides complete 4-phase compilation pipeline with pluggable backends
    
    use lexer_core, only: token_t, tokenize_core
    use parser_core, only: parse_expression, parse_statement, parser_state_t, &
                          create_parser_state
    use ast_core
    use ast_lazy_fortran
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                analyze_program
    use codegen_core, only: generate_code, generate_code_polymorphic
    use json_writer, only: json_write_tokens_to_file, json_write_ast_to_file
    
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
        type(semantic_context_t) :: sem_ctx
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
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, ast_tree)
        
        ! ============================
        ! PHASE 4: CODE GENERATION
        ! ============================
        select case (options%backend)
        case (BACKEND_FORTRAN)
            call generate_fortran_code(ast_tree, generated_code)
            
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
        class(ast_node), allocatable :: body(:), stmt
        type(lf_program_node) :: program
        integer :: stmt_count, body_capacity, i
        
        error_msg = ""
        stmt_count = 0
        body_capacity = 0
        
        ! Initialize parser
        parser = create_parser_state(tokens)
        
        ! Parse all statements
        do while (parser%current_token <= size(tokens))
            ! Parse single statement
            stmt = parse_statement(parser%tokens(parser%current_token:))
            
            if (allocated(stmt)) then
                stmt_count = stmt_count + 1
                
                ! Handle initial allocation
                if (.not. allocated(body)) then
                    allocate(body(10), source=stmt)
                    body_capacity = 10
                else if (stmt_count > body_capacity) then
                    ! Grow array if needed
                    call grow_ast_array(body, body_capacity * 2)
                    body_capacity = body_capacity * 2
                end if
                
                ! Add statement to body
                if (stmt_count <= body_capacity) then
                    allocate(body(stmt_count), source=stmt)
                end if
            end if
            
            ! Move to next statement (simplified - assumes one per line)
            call advance_to_next_statement(parser)
        end do
        
        ! Create program AST
        program%name = "main"
        program%implicit = .true.
        program%auto_contains = .false.
        if (stmt_count > 0) then
            allocate(program%body(stmt_count), source=body(1))
            do i = 2, stmt_count
                allocate(program%body(i), source=body(i))
            end do
        end if
        
        allocate(ast_tree, source=program)
        
    end subroutine parse_tokens
    
    ! Generate Fortran code from AST
    subroutine generate_fortran_code(ast_tree, code)
        class(ast_node), intent(in) :: ast_tree
        character(len=:), allocatable, intent(out) :: code
        
        select type (ast_tree)
        type is (lf_program_node)
            code = generate_fortran_program(ast_tree)
        class default
            code = "! Unsupported AST root type"
        end select
        
    end subroutine generate_fortran_code
    
    ! Generate complete Fortran program
    function generate_fortran_program(prog) result(code)
        type(lf_program_node), intent(in) :: prog
        character(len=:), allocatable :: code
        character(len=:), allocatable :: declarations, statements
        integer :: i
        
        ! Generate program header
        code = "program " // prog%name // new_line('a')
        code = code // "    implicit none" // new_line('a')
        
        ! Generate variable declarations from type information
        declarations = generate_declarations(prog)
        if (len_trim(declarations) > 0) then
            code = code // declarations // new_line('a')
        end if
        
        ! Generate executable statements
        if (allocated(prog%body)) then
            do i = 1, size(prog%body)
                code = code // "    " // generate_code_polymorphic(prog%body(i)) // new_line('a')
            end do
        end if
        
        ! Generate program footer
        code = code // "end program " // prog%name
        
    end function generate_fortran_program
    
    ! Generate variable declarations from AST with type info
    function generate_declarations(prog) result(decls)
        type(lf_program_node), intent(in) :: prog
        character(len=:), allocatable :: decls
        
        ! TODO: Implement declaration generation from inferred types
        decls = ""
        
    end function generate_declarations
    
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
    
end module frontend