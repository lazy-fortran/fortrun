module preprocessor
    ! AST-based preprocessor for Simple Fortran (.f files)
    use lexer_core, only: token_t, tokenize_core, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_KEYWORD
    use parser_core, only: parse_expression, parse_statement, parser_state_t, create_parser_state  
    use ast_core
    use codegen_core
    use ast_simple_fortran, only: sf_program_node, sf_assignment_node, inferred_var_node, create_sf_program
    implicit none
    private
    
    public :: preprocess_file, is_preprocessor_file, preprocess_file_debug
    
    ! Internal types for tracking functions/subroutines
    type :: function_info
        character(len=256) :: name
        character(len=256), allocatable :: lines(:)
        integer :: line_count
        logical :: is_function  ! true for function, false for subroutine
    end type function_info
    
contains

    function is_preprocessor_file(filename) result(is_dot_f)
        character(len=*), intent(in) :: filename
        logical :: is_dot_f
        integer :: ext_pos
        
        ext_pos = index(filename, '.', back=.true.)
        if (ext_pos > 0) then
            is_dot_f = filename(ext_pos:) == '.f'
        else
            is_dot_f = .false.
        end if
    end function is_preprocessor_file

    subroutine preprocess_file_debug(input_file, output_file, error_msg, debug_tokens, debug_ast, debug_codegen)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        logical, intent(in) :: debug_tokens, debug_ast, debug_codegen
        
        ! TODO: Implement debug JSON output for each pipeline stage
        ! For now, call the regular version
        call preprocess_file(input_file, output_file, error_msg)
        
        if (debug_tokens) then
            print *, "[DEBUG] Tokens JSON output would go here"
        end if
        if (debug_ast) then
            print *, "[DEBUG] AST JSON output would go here"
        end if
        if (debug_codegen) then
            print *, "[DEBUG] Codegen JSON output would go here"
        end if
    end subroutine preprocess_file_debug

    subroutine preprocess_file(input_file, output_file, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: generated_code
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_tree
        integer :: unit_in, unit_out, ios
        integer :: file_size
        character(len=1024) :: line
        
        error_msg = ""
        
        
        ! Process line by line instead of reading entire file
        open(newunit=unit_in, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            error_msg = "Failed to open input file: " // trim(input_file)
            return
        end if
        
        ! Open output file
        open(newunit=unit_out, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            error_msg = "Failed to create output file: " // trim(output_file)
            close(unit_in)
            return
        end if
        
        ! First pass: collect all lines and separate functions/subroutines from main code
        block
            character(len=256), dimension(500) :: lines
            integer :: line_count, i
            character(len=64), dimension(100) :: var_names
            character(len=32), dimension(100) :: var_types
            integer :: var_count
            character(len=256), dimension(50) :: use_statements
            integer :: use_count
            logical, dimension(500) :: line_processed
            type(function_info), dimension(50) :: functions
            integer :: func_count
            logical :: in_function
            integer :: current_func
            
            line_count = 0
            var_count = 0
            use_count = 0
            func_count = 0
            in_function = .false.
            current_func = 0
            line_processed = .false.
            
            ! Read all lines
            do
                read(unit_in, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (line_count >= 500) exit
                line_count = line_count + 1
                lines(line_count) = line
            end do
            close(unit_in)
            
            ! First pass: separate functions/subroutines from main code
            i = 1
            do while (i <= line_count)
                if (len_trim(lines(i)) > 0) then
                    source_code = trim(adjustl(lines(i)))
                    call tokenize_core(source_code, tokens)
                    
                    if (size(tokens) >= 2) then
                        ! Check for function/subroutine start
                        block
                            logical :: is_func_or_sub
                            is_func_or_sub = .false.
                            
                            ! Pattern 1: "function name" or "subroutine name"
                            if (tokens(1)%kind == TK_KEYWORD .and. &
                                (tokens(1)%text == "function" .or. tokens(1)%text == "subroutine")) then
                                is_func_or_sub = .true.
                            end if
                            
                            ! Pattern 2: "type function name" (e.g., "real function square")
                            if (.not. is_func_or_sub .and. size(tokens) >= 3) then
                                if (tokens(2)%kind == TK_KEYWORD .and. tokens(2)%text == "function") then
                                    is_func_or_sub = .true.
                                end if
                            end if
                            
                            if (is_func_or_sub) then
                            
                            ! Start collecting function/subroutine
                            func_count = func_count + 1
                            current_func = func_count
                            allocate(functions(current_func)%lines(100))
                            functions(current_func)%line_count = 0
                            in_function = .true.
                            
                            ! Determine if it's a function or subroutine
                            block
                                integer :: j
                                functions(current_func)%is_function = .false.
                                do j = 1, size(tokens)
                                    if (tokens(j)%text == "function") then
                                        functions(current_func)%is_function = .true.
                                        exit
                                    end if
                                end do
                            end block
                                
                            ! Get function/subroutine name
                            block
                                integer :: j
                                do j = 1, size(tokens)
                                    if (tokens(j)%text == "function" .or. &
                                        tokens(j)%text == "subroutine") then
                                        if (j < size(tokens)) then
                                            functions(current_func)%name = tokens(j+1)%text
                                        end if
                                        exit
                                    end if
                                end do
                            end block
                            else if (in_function .and. tokens(1)%kind == TK_KEYWORD .and. &
                                    tokens(1)%text == "end") then
                                ! Check if this ends the function/subroutine
                                if (size(tokens) >= 2 .and. &
                                    (tokens(2)%text == "function" .or. &
                                     tokens(2)%text == "subroutine")) then
                                    ! Add the end line and stop collecting
                                    functions(current_func)%line_count = &
                                        functions(current_func)%line_count + 1
                                    functions(current_func)%lines(functions(current_func)%line_count) = &
                                        lines(i)
                                    in_function = .false.
                                    current_func = 0
                                    i = i + 1
                                    cycle
                                end if
                            end if
                        end block
                    end if
                    
                    if (in_function) then
                        ! Add line to current function
                        functions(current_func)%line_count = functions(current_func)%line_count + 1
                        functions(current_func)%lines(functions(current_func)%line_count) = lines(i)
                    else if (.not. in_function .and. size(tokens) >= 1) then
                        ! Check for USE statement
                        if (tokens(1)%kind == TK_KEYWORD .and. tokens(1)%text == "use") then
                            use_count = use_count + 1
                            use_statements(use_count) = trim(lines(i))
                            line_processed(i) = .true.  ! Mark as processed
                        ! Check for assignment pattern in main code
                        else if (size(tokens) >= 3 .and. tokens(1)%kind == TK_IDENTIFIER .and. &
                            tokens(2)%kind == TK_OPERATOR .and. &
                            tokens(2)%text == "=") then
                            
                            ! Check if variable already tracked
                            block
                                integer :: j
                                logical :: found
                                found = .false.
                                do j = 1, var_count
                                    if (var_names(j) == tokens(1)%text) then
                                        found = .true.
                                        exit
                                    end if
                                end do
                                
                                if (.not. found) then
                                    var_count = var_count + 1
                                    var_names(var_count) = tokens(1)%text
                                    
                                    ! Simple type inference based on literal
                                    if (size(tokens) >= 3 .and. tokens(3)%kind == TK_NUMBER) then
                                        if (index(tokens(3)%text, ".") > 0) then
                                            var_types(var_count) = "real(8)"
                                        else
                                            var_types(var_count) = "integer"
                                        end if
                                    else
                                        var_types(var_count) = "real(8)"  ! Default to real(8)
                                    end if
                                end if
                            end block
                        end if
                    end if
                end if
                i = i + 1
            end do
            
            ! Write program header
            write(unit_out, '(A)') "program main"
            
            ! Write USE statements
            do i = 1, use_count
                write(unit_out, '(A,A)') "    ", trim(use_statements(i))
            end do
            
            write(unit_out, '(A)') "    implicit none"
            
            ! Write variable declarations
            do i = 1, var_count
                write(unit_out, '(A,A,A,A)') "    ", trim(var_types(i)), " :: ", trim(var_names(i))
            end do
            
            if (var_count > 0) then
                write(unit_out, '(A)') ""  ! Blank line after declarations
            end if
            
            ! Process main code lines (not in functions)
            in_function = .false.
            do i = 1, line_count
                if (len_trim(lines(i)) > 0 .and. .not. line_processed(i)) then
                    source_code = trim(adjustl(lines(i)))
                    call tokenize_core(source_code, tokens)
                    
                    ! Skip function/subroutine lines
                    if (size(tokens) >= 2) then
                        block
                            logical :: is_func_or_sub
                            is_func_or_sub = .false.
                            
                            ! Pattern 1: "function name" or "subroutine name"
                            if (tokens(1)%kind == TK_KEYWORD .and. &
                                (tokens(1)%text == "function" .or. tokens(1)%text == "subroutine")) then
                                is_func_or_sub = .true.
                            end if
                            
                            ! Pattern 2: "type function name" (e.g., "real function square")
                            if (.not. is_func_or_sub .and. size(tokens) >= 3) then
                                if (tokens(2)%kind == TK_KEYWORD .and. tokens(2)%text == "function") then
                                    is_func_or_sub = .true.
                                end if
                            end if
                            
                            if (is_func_or_sub) then
                                in_function = .true.
                            else if (in_function .and. tokens(1)%kind == TK_KEYWORD .and. &
                                    tokens(1)%text == "end" .and. size(tokens) >= 2 .and. &
                                    (tokens(2)%text == "function" .or. tokens(2)%text == "subroutine")) then
                                in_function = .false.
                                cycle
                            end if
                        end block
                    end if
                    
                    if (.not. in_function .and. size(tokens) > 0) then
                        ! Skip USE statements (already handled)
                        if (.not. (tokens(1)%kind == TK_KEYWORD .and. tokens(1)%text == "use")) then
                            ! Try AST-based processing first, fallback to simple reconstruction
                            generated_code = process_statement_ast_or_fallback(tokens)
                            if (len_trim(generated_code) > 0) then
                                write(unit_out, '(A,A)') "    ", trim(generated_code)
                            end if
                        end if
                    end if
                end if
            end do
            
            ! Write contains section if we have functions/subroutines
            if (func_count > 0) then
                write(unit_out, '(A)') "contains"
                
                ! Process each function/subroutine
                do i = 1, func_count
                    call process_function(unit_out, functions(i))
                end do
            end if
            
            ! Write program footer
            write(unit_out, '(A)') "end program main"
        end block
        
        close(unit_in)
        close(unit_out)
        return
    end subroutine preprocess_file
    
    ! Process a single line and generate code
    function process_statement_ast_or_fallback(tokens) result(code)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: code
        
        ! Try to parse as AST statement for supported features
        ! For now, try assignment statements
        if (size(tokens) >= 3 .and. &
            tokens(1)%kind == TK_IDENTIFIER .and. &
            tokens(2)%kind == TK_OPERATOR .and. &
            tokens(2)%text == "=") then
            
            ! Try AST-based assignment processing
            code = process_assignment_ast(tokens)
            if (len_trim(code) > 0) return
        end if
        
        ! Fallback: trivial 1:1 reconstruction with no logic
        code = process_line_simple(tokens)
    end function process_statement_ast_or_fallback

    function process_assignment_ast(tokens) result(code)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: code
        
        ! TODO: Complete AST pipeline implementation
        ! For now, return empty to trigger fallback until interface issues are resolved
        code = ""
        
    end function process_assignment_ast

    function process_line_simple(tokens) result(code)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: code
        integer :: i
        
        ! Trivial fallback: just reconstruct the line 1:1 with no logic
        code = ""
        do i = 1, size(tokens)
            if (i > 1) code = code // " "
            code = code // tokens(i)%text
        end do
    end function process_line_simple
    
    ! Parse Simple Fortran (for now, just parse individual statements)
    function parse_simple_fortran(tokens) result(ast)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: ast
        
        ! For now, just parse as an expression or statement
        ! In the future, this will handle the full Simple Fortran dialect
        if (size(tokens) > 2) then
            ! Try to parse as statement first
            ast = parse_statement(tokens)
        else
            ! Try as expression
            ast = parse_expression(tokens)
        end if
        
    end function parse_simple_fortran
    
    ! Transform AST for Simple Fortran semantics
    function transform_simple_fortran_ast(ast) result(new_ast)
        class(ast_node), intent(in) :: ast
        class(ast_node), allocatable :: new_ast
        type(sf_program_node) :: sf_prog
        class(ast_node), allocatable :: body(:)
        
        ! For now, wrap single statements in a program
        select type (ast)
        type is (assignment_node)
            ! Create a Simple Fortran program with the assignment
            allocate(body(1), source=ast)
            sf_prog = create_sf_program("main", body, implicit=.true., auto_contains=.false.)
            
            ! Return the wrapped program
            allocate(new_ast, source=sf_prog)
            
        class default
            ! For other node types, return as-is for now
            allocate(new_ast, source=ast)
        end select
        
    end function transform_simple_fortran_ast
    
    ! Generate Fortran code from AST
    function generate_fortran_code(ast) result(code)
        class(ast_node), intent(in) :: ast
        character(len=:), allocatable :: code
        
        ! Generate code using the codegen module
        select type (ast)
        type is (sf_program_node)
            code = generate_code(ast)
        type is (assignment_node)
            ! Wrap in a simple program
            code = "program main" // new_line('a')
            code = code // "    implicit none" // new_line('a')
            code = code // "    " // generate_code(ast) // new_line('a')
            code = code // "end program main"
        class default
            ! For other nodes, generate simple code
            code = "! Preprocessed code" // new_line('a')
        end select
        
    end function generate_fortran_code
    
    ! Combined parse, transform and generate to avoid polymorphic assignment issues
    function parse_and_generate_simple_fortran(tokens) result(code)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: code
        class(ast_node), allocatable :: body(:)
        type(sf_program_node) :: sf_prog
        integer :: i
        logical :: has_equals
        
        ! Check if this is an assignment statement
        has_equals = .false.
        do i = 1, size(tokens)
            if (tokens(i)%text == "=") then
                has_equals = .true.
                exit
            end if
        end do
        
        if (has_equals .and. size(tokens) > 2) then
            ! Parse as assignment and wrap in program
            code = "program main" // new_line('a')
            code = code // "    implicit none" // new_line('a')
            
            ! Generate the assignment directly without going through AST
            ! This is a temporary solution to avoid polymorphic assignment issues
            block
                type(parser_state_t) :: parser
                type(token_t) :: id_token, op_token
                character(len=:), allocatable :: target_name, value_expr
                
                parser = create_parser_state(tokens)
                
                ! Get identifier
                id_token = parser%consume()
                target_name = id_token%text
                
                ! Skip =
                op_token = parser%consume()
                
                ! Get rest as value expression
                value_expr = ""
                do while (.not. parser%is_at_end())
                    block
                        type(token_t) :: tok
                        tok = parser%consume()
                        if (len(value_expr) > 0) value_expr = value_expr // " "
                        value_expr = value_expr // tok%text
                    end block
                end do
                
                code = code // "    " // target_name // " = " // value_expr // new_line('a')
            end block
            
            code = code // "end program main"
        else
            ! For other cases, generate a simple program
            code = "program main" // new_line('a')
            code = code // "    implicit none" // new_line('a')
            code = code // "    ! Expression: " // tokens_to_string(tokens) // new_line('a')
            code = code // "end program main"
        end if
        
    end function parse_and_generate_simple_fortran
    
    ! Helper function to convert tokens to string
    function tokens_to_string(tokens) result(str)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: str
        integer :: i
        
        str = ""
        do i = 1, size(tokens)
            if (i > 1) str = str // " "
            str = str // tokens(i)%text
        end do
    end function tokens_to_string
    
    ! Process a function or subroutine, adding intent(in) to parameters
    subroutine process_function(unit, func_info)
        integer, intent(in) :: unit
        type(function_info), intent(in) :: func_info
        character(len=1024) :: line
        type(token_t), allocatable :: tokens(:)
        integer :: i, j
        logical :: in_params, processing_decl
        character(len=:), allocatable :: processed_line
        
        do i = 1, func_info%line_count
            line = func_info%lines(i)
            
            if (len_trim(line) > 0) then
                call tokenize_core(trim(adjustl(line)), tokens)
                
                ! Check if this is a parameter declaration line
                processing_decl = .false.
                if (size(tokens) >= 2) then
                    ! Look for type declarations (real, integer, etc.)
                    if (tokens(1)%kind == TK_IDENTIFIER .and. &
                        (tokens(1)%text == "real" .or. &
                         tokens(1)%text == "integer" .or. &
                         tokens(1)%text == "logical" .or. &
                         tokens(1)%text == "character")) then
                        processing_decl = .true.
                    end if
                end if
                
                if (processing_decl) then
                    ! Check if this declaration already has intent
                    block
                        logical :: has_intent
                        has_intent = .false.
                        do j = 1, size(tokens)
                            if (tokens(j)%text == "intent") then
                                has_intent = .true.
                                exit
                            end if
                        end do
                        
                        if (.not. has_intent) then
                            ! Add intent(in) after the type
                            processed_line = "    " // tokens(1)%text // ", intent(in)"
                            
                            ! Add rest of the line
                            do j = 2, size(tokens)
                                processed_line = processed_line // " " // tokens(j)%text
                            end do
                            
                            write(unit, '(A)') trim(processed_line)
                        else
                            ! Keep the line as-is but indented
                            write(unit, '(A,A)') "  ", trim(line)
                        end if
                    end block
                else
                    ! Other lines - just indent
                    write(unit, '(A,A)') "  ", trim(line)
                end if
            else
                write(unit, '(A)') ""  ! Blank line
            end if
        end do
    end subroutine process_function

end module preprocessor