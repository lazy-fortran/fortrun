module preprocessor_ast
    ! AST-based preprocessor for Simple Fortran (.f files)
    use lexer_core, only: token_t, tokenize_core
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
        
        
        ! Read entire source file
        open(newunit=unit_in, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            error_msg = "Failed to open input file: " // trim(input_file)
            return
        end if
        
        ! Get file size and read content
        inquire(unit=unit_in, size=file_size)
        allocate(character(len=file_size) :: source_code)
        source_code = ""
        
        do
            read(unit_in, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(source_code) > 0) then
                source_code = source_code // new_line('a') // trim(line)
            else
                source_code = trim(line)
            end if
        end do
        close(unit_in)
        
        ! Step 1: Tokenize
        call tokenize_core(source_code, tokens)
        
        ! Step 2-4: Parse, transform and generate in one step
        ! This avoids issues with polymorphic assignment of allocatable components
        generated_code = parse_and_generate_simple_fortran(tokens)
        
        ! Write output file
        open(newunit=unit_out, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            error_msg = "Failed to create output file: " // trim(output_file)
            return
        end if
        
        write(unit_out, '(A)') generated_code
        close(unit_out)
        
    end subroutine preprocess_file_ast
    
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