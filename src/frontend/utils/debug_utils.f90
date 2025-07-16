module debug_utils
    ! Debug output utilities for frontend debugging
    ! Handles JSON output for tokens, AST, and code generation stages

    use lexer_core, only: token_t
    use ast_core
    use json_writer, only: json_write_tokens_to_file, json_write_ast_to_file
    implicit none
    private

    ! Public interface
    public :: debug_output_tokens
    public :: debug_output_ast
    public :: debug_output_semantic
    public :: debug_output_codegen

contains

    ! Debug output functions
    subroutine debug_output_tokens(input_file, tokens)
        character(len=*), intent(in) :: input_file
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: json_file

        json_file = input_file
        if (index(json_file, '.') > 0) then
            json_file = json_file(1:index(json_file, '.', back=.true.) - 1)
        end if
        json_file = json_file//"_tokens.json"

        print *, "DEBUG: Writing tokens to: ", trim(json_file)
        call json_write_tokens_to_file(tokens, json_file)
    end subroutine debug_output_tokens

    subroutine debug_output_ast(input_file, ast_tree)
        use json_writer, only: json_write_ast_to_file
        character(len=*), intent(in) :: input_file
        class(ast_node), intent(in) :: ast_tree
        character(len=:), allocatable :: json_file

        ! Create AST filename in same directory as input file
        json_file = input_file
        if (index(json_file, '.') > 0) then
            json_file = json_file(1:index(json_file, '.', back=.true.) - 1)
        end if
        json_file = json_file//"_ast.json"

        print *, "DEBUG: Writing AST to: ", trim(json_file)

        ! Use json_writer for proper AST serialization
        call json_write_ast_to_file(ast_tree, json_file)
    end subroutine debug_output_ast

    subroutine debug_output_semantic(input_file, ast)
        use ast_core, only: ast_node
        use json_writer, only: json_write_ast_to_file
        character(len=*), intent(in) :: input_file
        class(ast_node), intent(in) :: ast
        character(len=:), allocatable :: json_file

        ! Create semantic filename in same directory as input file
        json_file = input_file
        if (index(json_file, '.') > 0) then
            json_file = json_file(1:index(json_file, '.', back=.true.) - 1)
        end if
        json_file = json_file//"_semantic.json"

        print *, "DEBUG: Writing semantic analysis output to: ", trim(json_file)

        ! Use json_writer for AST serialization (now with type annotations)
        call json_write_ast_to_file(ast, json_file)
    end subroutine debug_output_semantic

    subroutine debug_output_codegen(input_file, code)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: code
        character(len=:), allocatable :: json_file
        integer :: unit, i, line_start, line_end

        ! Create codegen filename in same directory as input file
        json_file = input_file
        if (index(json_file, '.') > 0) then
            json_file = json_file(1:index(json_file, '.', back=.true.) - 1)
        end if
        json_file = json_file//"_codegen.json"

        ! Write JSON with generated code
        open (newunit=unit, file=json_file, status='replace', action='write')
        write (unit, '(a)') '{'
        write (unit, '(a,a,a)') '  "input_file": "', trim(input_file), '",'
        write (unit, '(a)') '  "generated_code": ['

        ! Write code line by line as JSON array
        line_start = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a') .or. i == len(code)) then
                line_end = i - 1
                if (i == len(code) .and. code(i:i) /= new_line('a')) line_end = i

                ! Write line with proper JSON escaping
                write (unit, '(a)', advance='no') '    "'
                ! Basic JSON escaping for quotes
                block
                    integer :: j
                    do j = line_start, line_end
                        if (code(j:j) == '"') then
                            write (unit, '(a)', advance='no') '\"'
                        else if (code(j:j) == '\') then
                            write (unit, '(a)', advance='no') '\\'
                        else
                            write (unit, '(a)', advance='no') code(j:j)
                        end if
                    end do
                end block
                write (unit, '(a)', advance='no') '"'

                ! Add comma if not last line
                if (i < len(code)) write (unit, '(a)', advance='no') ','
                write (unit, '(a)') ''

                line_start = i + 1
            end if
        end do

        write (unit, '(a)') '  ]'
        write (unit, '(a)') '}'
        close (unit)
    end subroutine debug_output_codegen

end module debug_utils
