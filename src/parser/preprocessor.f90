module preprocessor
    ! AST-based preprocessor for Simple Fortran (.f files)
    use lexer_core, only: token_t, tokenize_core, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_KEYWORD, TK_STRING, TK_EOF
    use parser_core, only: parse_expression, parse_statement, parser_state_t, create_parser_state  
    use ast_core
    use codegen_core
    use ast_simple_fortran, only: sf_program_node, sf_assignment_node, inferred_var_node, create_sf_program
    use json_writer, only: json_write_tokens_to_file, json_write_ast_to_file
    implicit none
    private
    
    public :: preprocess_file, is_preprocessor_file, preprocess_file_debug, preprocess_file_ast_based
    
    ! Internal types for tracking functions/subroutines
    type :: function_info
        character(len=256) :: name
        character(len=256), allocatable :: lines(:)
        integer :: line_count
        logical :: is_function  ! true for function, false for subroutine
    end type function_info
    
contains

    ! Find position of comment marker, accounting for strings
    function find_comment_position(line) result(pos)
        character(len=*), intent(in) :: line
        integer :: pos
        integer :: i
        logical :: in_string
        character :: quote_char
        
        pos = 0
        in_string = .false.
        quote_char = ' '
        
        do i = 1, len_trim(line)
            if (.not. in_string) then
                ! Check for string start
                if (line(i:i) == '"' .or. line(i:i) == "'") then
                    in_string = .true.
                    quote_char = line(i:i)
                ! Check for comment start
                else if (line(i:i) == '!') then
                    pos = i
                    return
                end if
            else
                ! Check for string end
                if (line(i:i) == quote_char) then
                    in_string = .false.
                    quote_char = ' '
                end if
            end if
        end do
        
        pos = 0  ! No comment found
    end function find_comment_position

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
        
        character(len=1024) :: line
        character(len=1024), allocatable :: lines(:)
        integer :: line_count, i, unit, ios
        type(token_t), allocatable :: tokens(:), all_tokens(:)
        class(ast_node), allocatable :: stmt_ast
        character(len=:), allocatable :: debug_base_name
        integer :: total_tokens
        
        ! First, call the regular preprocessing
        call preprocess_file(input_file, output_file, error_msg)
        if (len_trim(error_msg) > 0) return
        
        ! Get base name for debug files
        debug_base_name = input_file
        if (index(debug_base_name, '.f') > 0) then
            debug_base_name = debug_base_name(1:index(debug_base_name, '.f')-1)
        end if
        
        if (debug_tokens .or. debug_ast .or. debug_codegen) then
            ! Read the input file for debug processing
            allocate(lines(500))
            line_count = 0
            total_tokens = 0
            
            open(newunit=unit, file=input_file, status='old', action='read', iostat=ios)
            if (ios /= 0) then
                error_msg = "Failed to open input file for debug: " // trim(input_file)
                return
            end if
            
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (line_count >= 500) exit
                line_count = line_count + 1
                lines(line_count) = line
            end do
            close(unit)
            
            ! Tokenize all lines for debug output
            if (debug_tokens) then
                ! Collect all tokens from all lines
                allocate(all_tokens(0))
                
                do i = 1, line_count
                    line = trim(lines(i))
                    if (len_trim(line) == 0) cycle
                    if (line(1:1) == '!') cycle
                    
                    ! Remove inline comments (accounting for strings)
                    block
                        integer :: comment_pos
                        comment_pos = find_comment_position(line)
                        if (comment_pos > 0) then
                            line = trim(line(1:comment_pos-1))
                            if (len_trim(line) == 0) cycle
                        end if
                    end block
                    
                    call tokenize_core(line, tokens)
                    if (size(tokens) > 0) then
                        ! Append tokens to all_tokens array
                        block
                            type(token_t), allocatable :: temp_tokens(:)
                            integer :: old_size, j
                            old_size = size(all_tokens)
                            allocate(temp_tokens(old_size + size(tokens)))
                            do j = 1, old_size
                                temp_tokens(j) = all_tokens(j)
                            end do
                            do j = 1, size(tokens)
                                temp_tokens(old_size + j) = tokens(j)
                            end do
                            call move_alloc(temp_tokens, all_tokens)
                        end block
                    end if
                end do
                
                ! Write tokens to JSON file
                call json_write_tokens_to_file(all_tokens, trim(debug_base_name) // "_tokens.json")
                print *, "[DEBUG] Tokens written to: " // trim(debug_base_name) // "_tokens.json"
            end if
            
            if (debug_ast) then
                ! Parse a simple statement for demonstration
                ! Full AST parsing will be implemented in next phase
                block
                    class(ast_node), allocatable :: simple_ast
                    
                    ! Find first non-comment, non-empty line and parse it
                    do i = 1, line_count
                        line = trim(lines(i))
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) == '!') cycle
                        
                        ! Remove inline comments
                        block
                            integer :: comment_pos
                            comment_pos = index(line, '!')
                            if (comment_pos > 0) then
                                line = trim(line(1:comment_pos-1))
                                if (len_trim(line) == 0) cycle
                            end if
                        end block
                        
                        call tokenize_core(line, tokens)
                        if (size(tokens) > 1) then  ! Skip EOF-only lines
                            simple_ast = parse_statement(tokens)
                            if (allocated(simple_ast)) then
                                call json_write_ast_to_file(simple_ast, trim(debug_base_name) // "_ast.json")
                                print *, "[DEBUG] AST written to: " // trim(debug_base_name) // "_ast.json"
                                exit
                            end if
                        end if
                    end do
                    
                    if (.not. allocated(simple_ast)) then
                        print *, "[DEBUG] No parseable AST nodes found"
                    end if
                end block
            end if
        end if
        
        if (debug_codegen) then
            ! Generate code from AST and serialize to JSON
            block
                use json_module
                type(json_core) :: json
                type(json_value), pointer :: root, codegen_obj
                class(ast_node), allocatable :: simple_ast
                character(len=:), allocatable :: generated_code
                integer :: i
                
                ! Initialize JSON
                call json%initialize()
                call json%create_object(root, '')
                
                ! Find first non-comment, non-empty line and generate code
                do i = 1, line_count
                    line = trim(lines(i))
                    if (len_trim(line) == 0) cycle
                    if (line(1:1) == '!') cycle
                    
                    ! Remove inline comments (accounting for strings)
                    block
                        integer :: comment_pos
                        comment_pos = find_comment_position(line)
                        if (comment_pos > 0) then
                            line = trim(line(1:comment_pos-1))
                            if (len_trim(line) == 0) cycle
                        end if
                    end block
                    
                    call tokenize_core(line, tokens)
                    if (size(tokens) > 1) then  ! Skip EOF-only lines
                        simple_ast = parse_statement(tokens)
                        if (allocated(simple_ast)) then
                            ! Generate code from AST
                            select type (simple_ast)
                            type is (sf_assignment_node)
                                generated_code = generate_code(simple_ast)
                            type is (assignment_node)
                                generated_code = generate_code(simple_ast)
                            class default
                                generated_code = "! Unsupported AST node type"
                            end select
                            
                            ! Create codegen entry
                            call json%create_object(codegen_obj, 'codegen')
                            call json%add(codegen_obj, 'input_line', line)
                            call json%add(codegen_obj, 'generated_code', generated_code)
                            call json%add(codegen_obj, 'line_number', i)
                            call json%add(root, codegen_obj)
                            
                            ! Only process first statement for now
                            exit
                        end if
                    end if
                end do
                
                ! Write to file
                call json%print(root, trim(debug_base_name) // "_codegen.json")
                print *, "[DEBUG] Codegen written to: " // trim(debug_base_name) // "_codegen.json"
                
                ! Clean up
                call json%destroy(root)
            end block
        end if
    end subroutine preprocess_file_debug

    ! NEW: Main preprocessor now uses AST-based implementation by default
    subroutine preprocess_file(input_file, output_file, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        
        ! Use AST-based preprocessor as default
        ! print *, "DEBUG: preprocess_file calling preprocess_file_ast_based"
        call preprocess_file_ast_based(input_file, output_file, error_msg)
    end subroutine preprocess_file

    subroutine preprocess_file_legacy(input_file, output_file, error_msg)
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
                            else if (in_function .and. size(tokens) >= 1 .and. &
                                    tokens(1)%kind == TK_KEYWORD .and. &
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
                            else if (in_function .and. size(tokens) >= 2 .and. &
                                    tokens(1)%kind == TK_KEYWORD .and. &
                                    tokens(1)%text == "end" .and. &
                                    (tokens(2)%text == "function" .or. tokens(2)%text == "subroutine")) then
                                in_function = .false.
                                cycle
                            end if
                        end block
                    end if
                    
                    if (.not. in_function .and. size(tokens) > 0) then
                        ! Skip USE statements (already handled)
                        if (.not. (size(tokens) >= 1 .and. tokens(1)%kind == TK_KEYWORD .and. tokens(1)%text == "use")) then
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
    end subroutine preprocess_file_legacy
    
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

    ! NEW AST-based preprocessing function (selective implementation)
    subroutine preprocess_file_ast_based(input_file, output_file, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        
        character(len=:), allocatable :: source_code
        character(len=1024), allocatable :: lines(:)
        integer :: line_count, i, unit, ios
        character(len=1024) :: line
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt_ast
        character(len=:), allocatable :: generated_code
        character(len=256), allocatable :: var_names(:)
        character(len=32), allocatable :: var_types(:)
        integer :: var_count
        character(len=256), allocatable :: use_statements(:)
        integer :: use_count
        
        error_msg = ""
        var_count = 0
        use_count = 0
        allocate(var_names(100), var_types(100), use_statements(50))
        
        ! Read all lines first
        allocate(lines(500))
        line_count = 0
        
        open(newunit=unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            error_msg = "Failed to open input file: " // trim(input_file)
            return
        end if
        
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (line_count >= 500) exit
            line_count = line_count + 1
            lines(line_count) = line
        end do
        close(unit)
        
        ! Open output file
        open(newunit=unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            error_msg = "Failed to create output file: " // trim(output_file)
            return
        end if
        
        ! First pass: collect USE statements and process other statements
        do i = 1, line_count
            line = trim(lines(i))
            if (len_trim(line) == 0) cycle
            
            ! Skip comment lines
            if (line(1:1) == '!') cycle
            
            ! Remove inline comments before tokenizing (accounting for strings)
            block
                integer :: comment_pos
                comment_pos = find_comment_position(line)
                if (comment_pos > 0) then
                    line = trim(line(1:comment_pos-1))
                    if (len_trim(line) == 0) cycle
                end if
            end block
            
            ! Tokenize the line
            call tokenize_core(line, tokens)
            if (size(tokens) == 0) cycle
            ! Skip lines that only have EOF token (empty or comment-only lines)
            if (size(tokens) == 1 .and. tokens(1)%kind == TK_EOF) cycle
            
            ! Check for USE statement
            if (size(tokens) >= 2 .and. tokens(1)%kind == TK_KEYWORD .and. &
                tokens(1)%text == "use") then
                ! Handle USE statement via AST
                use_count = use_count + 1
                use_statements(use_count) = trim(line)
                
            else if (size(tokens) >= 3 .and. tokens(1)%kind == TK_IDENTIFIER .and. &
                tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == "=") then
                
                ! Track variable for type inference
                call track_variable_type(tokens(1)%text, tokens(3:), var_names, var_types, var_count)
                
            ! No special handling needed for print statements in first pass
            end if
        end do
        
        ! Write program header with proper ordering
        write(unit, '(A)') "program main"
        
        ! Write USE statements first (proper Fortran ordering)
        do i = 1, use_count
            write(unit, '(A,A)') "    ", trim(use_statements(i))
        end do
        
        write(unit, '(A)') "    implicit none"
        
        ! Write variable declarations
        do i = 1, var_count
            write(unit, '(A,A,A,A)') "    ", trim(var_types(i)), " :: ", trim(var_names(i))
        end do
        
        if (var_count > 0) write(unit, '(A)') ""
        
        ! Second pass: process executable statements
        do i = 1, line_count
            line = trim(lines(i))
            if (len_trim(line) == 0) cycle
            
            ! Skip comment lines
            if (line(1:1) == '!') cycle
            
            ! Remove inline comments before tokenizing (accounting for strings)
            block
                integer :: comment_pos
                comment_pos = find_comment_position(line)
                if (comment_pos > 0) then
                    line = trim(line(1:comment_pos-1))
                    if (len_trim(line) == 0) cycle
                end if
            end block
            
            ! Tokenize the line
            call tokenize_core(line, tokens)
            if (size(tokens) == 0) cycle
            ! Skip lines that only have EOF token (empty or comment-only lines)
            if (size(tokens) == 1 .and. tokens(1)%kind == TK_EOF) cycle
            
            ! Skip USE statements (already processed)
            if (size(tokens) >= 2 .and. tokens(1)%kind == TK_KEYWORD .and. &
                tokens(1)%text == "use") then
                cycle
            end if
            
            ! Process assignments using AST parser
            if (size(tokens) >= 3 .and. tokens(1)%kind == TK_IDENTIFIER .and. &
                tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == "=") then
                
                ! Parse assignment using proper AST parsing
                stmt_ast = parse_statement(tokens)
                
                ! Generate code from AST
                select type(stmt_ast)
                type is (assignment_node)
                    generated_code = generate_code(stmt_ast)
                    write(unit, '(A,A)') "    ", trim(generated_code)
                class default
                    ! Fallback to line reconstruction for unsupported AST nodes
                    write(unit, '(A,A)') "    ", trim(line)
                end select
                
            else if (size(tokens) >= 2 .and. tokens(1)%kind == TK_KEYWORD .and. &
                tokens(1)%text == "print") then
                
                ! Parse print statement using simple AST parsing
                call parse_print_statement_simple(tokens, stmt_ast)
                
                ! Generate code from AST
                select type(stmt_ast)
                type is (print_statement_node)
                    generated_code = generate_code(stmt_ast)
                    write(unit, '(A,A)') "    ", trim(generated_code)
                class default
                    ! Fallback to line reconstruction
                    write(unit, '(A,A)') "    ", trim(line)
                end select
                
            else
                ! Fallback to line reconstruction for unsupported features
                write(unit, '(A,A)') "    ", trim(line)
            end if
        end do
        
        write(unit, '(A)') "end program main"
        
        close(unit)
        
    contains
    
        subroutine track_variable_type(var_name, value_tokens, var_names, var_types, var_count)
            character(len=*), intent(in) :: var_name
            type(token_t), intent(in) :: value_tokens(:)
            character(len=256), intent(inout) :: var_names(:)
            character(len=32), intent(inout) :: var_types(:)
            integer, intent(inout) :: var_count
            integer :: j
            logical :: found
            
            ! Check if variable already exists
            found = .false.
            do j = 1, var_count
                if (trim(var_names(j)) == trim(var_name)) then
                    found = .true.
                    exit
                end if
            end do
            
            if (.not. found) then
                var_count = var_count + 1
                var_names(var_count) = var_name
                
                ! Simple type inference
                if (size(value_tokens) >= 1) then
                    if (value_tokens(1)%kind == TK_NUMBER) then
                        if (index(value_tokens(1)%text, ".") > 0) then
                            var_types(var_count) = "real(8)"
                        else
                            var_types(var_count) = "integer"
                        end if
                    else if (value_tokens(1)%kind == TK_STRING) then
                        ! Infer string length from literal (subtract 2 for quotes)
                        block
                            integer :: str_len
                            str_len = len_trim(value_tokens(1)%text) - 2
                            if (str_len < 1) str_len = 1
                            write(var_types(var_count), '(a,i0,a)') "character(len=", str_len, ")"
                        end block
                    else
                        var_types(var_count) = "real(8)"  ! Default for expressions
                    end if
                else
                    var_types(var_count) = "real(8)"  ! Default
                end if
            end if
        end subroutine track_variable_type
        
        subroutine parse_print_statement_simple(tokens, stmt_ast)
            type(token_t), intent(in) :: tokens(:)
            class(ast_node), allocatable, intent(out) :: stmt_ast
            character(len=:), allocatable :: format_spec
            
            ! Simple print statement parsing 
            ! For now, just create a basic print node without parsing arguments
            
            if (size(tokens) >= 3 .and. tokens(2)%text == "*") then
                ! print *, args...
                format_spec = "*"
            else
                ! print format, args or just print
                format_spec = ""
            end if
            
            ! For now, fallback to line reconstruction since we need argument parsing
            ! This is a placeholder for proper AST implementation
            ! In practice, this will trigger the fallback case
            allocate(stmt_ast, source=create_identifier("print_placeholder", 1, 1))
        end subroutine parse_print_statement_simple
        
    end subroutine preprocess_file_ast_based

end module preprocessor