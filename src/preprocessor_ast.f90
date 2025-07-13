module preprocessor_ast
    ! AST-based preprocessor for Simple Fortran (.f files)
    use lexer_core, only: token_t, tokenize_core, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER
    use parser_core, only: parse_expression, parse_statement, parser_state_t, create_parser_state  
    use ast_core
    use codegen_core
    use ast_simple_fortran, only: sf_program_node, sf_assignment_node, inferred_var_node, create_sf_program
    implicit none
    private
    
    public :: preprocess_file_ast
    
contains

    subroutine preprocess_file_ast(input_file, output_file, error_msg)
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
        
        ! First pass: collect all variable assignments for type inference
        block
            character(len=256), dimension(100) :: lines
            integer :: line_count, i
            character(len=64), dimension(100) :: var_names
            character(len=32), dimension(100) :: var_types
            integer :: var_count
            
            line_count = 0
            var_count = 0
            
            ! Read all lines
            do
                read(unit_in, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (line_count >= 100) exit
                line_count = line_count + 1
                lines(line_count) = line
            end do
            close(unit_in)
            
            ! Analyze lines for variable assignments
            do i = 1, line_count
                if (len_trim(lines(i)) > 0) then
                    source_code = trim(lines(i))
                    call tokenize_core(source_code, tokens)
                    
                    if (size(tokens) >= 3) then
                        ! Check for assignment pattern: IDENTIFIER = ...
                        if (tokens(1)%kind == TK_IDENTIFIER .and. &
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
                                            var_types(var_count) = "real"
                                        else
                                            var_types(var_count) = "integer"
                                        end if
                                    else
                                        var_types(var_count) = "real"  ! Default to real
                                    end if
                                end if
                            end block
                        end if
                    end if
                end if
            end do
            
            ! Write program header
            write(unit_out, '(A)') "program main"
            write(unit_out, '(A)') "    implicit none"
            
            ! Write variable declarations
            do i = 1, var_count
                write(unit_out, '(A,A,A,A)') "    ", trim(var_types(i)), " :: ", trim(var_names(i))
            end do
            
            if (var_count > 0) then
                write(unit_out, '(A)') ""  ! Blank line after declarations
            end if
            
            ! Process each line
            do i = 1, line_count
                if (len_trim(lines(i)) > 0) then
                    source_code = trim(lines(i))
                    call tokenize_core(source_code, tokens)
                    
                    if (size(tokens) > 0) then
                        ! Generate code for this line
                        generated_code = process_line_simple(tokens)
                        if (len_trim(generated_code) > 0) then
                            write(unit_out, '(A,A)') "    ", trim(generated_code)
                        end if
                    end if
                end if
            end do
            
            ! Write program footer
            write(unit_out, '(A)') "end program main"
        end block
        
        close(unit_in)
        close(unit_out)
        return
    end subroutine preprocess_file_ast
    
    ! Process a single line and generate code
    function process_line_simple(tokens) result(code)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: code
        integer :: i
        
        ! Just reconstruct the line for now
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

end module preprocessor_ast