module debug_utils
    ! Debug output utilities for frontend debugging
    ! Handles JSON output for tokens, AST, and code generation stages
    
    use lexer_core, only: token_t
    use ast_core
    use ast_lazy_fortran, only: lf_program_node
    use json_writer, only: json_write_tokens_to_file, json_write_ast_to_file
    implicit none
    private
    
    ! Public interface
    public :: debug_output_tokens
    public :: debug_output_ast
    public :: debug_output_codegen
    
contains

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
        
        ! Extract basename and create AST filename safely
        block
            integer :: dot_pos, slash_pos
            character(len=:), allocatable :: basename
            
            ! Find the last slash to get basename
            slash_pos = index(input_file, '/', back=.true.)
            if (slash_pos > 0) then
                basename = input_file(slash_pos+1:)
            else
                basename = input_file
            end if
            
            ! Remove extension if present
            dot_pos = index(basename, '.', back=.true.)
            if (dot_pos > 0) then
                basename = basename(1:dot_pos-1)
            end if
            
            json_file = trim(basename) // "_ast.json"
        end block
        
        ! Temporarily use simple manual JSON output due to json-fortran library issues
        block
            integer :: unit
            open(newunit=unit, file=json_file, status='replace', action='write')
            select type (ast => ast_tree)
            type is (lf_program_node)
                write(unit, '(a)') '{'
                write(unit, '(a)') '  "": {'
                write(unit, '(a)') '    "type": "lf_program",'
                write(unit, '(a,a,a)') '    "name": "', trim(ast%name), '",'
                write(unit, '(a,l1,a)') '    "implicit": ', ast%implicit, ','
                write(unit, '(a,l1,a)') '    "auto_contains": ', ast%auto_contains, ','
                write(unit, '(a,i0,a)') '    "line": ', ast%line, ','
                write(unit, '(a,i0,a)') '    "column": ', ast%column, ','
                write(unit, '(a)') '    "body": []'
                write(unit, '(a)') '  }'
                write(unit, '(a)') '}'
            class default
                write(unit, '(a)') '{"error": "Unknown AST node type"}'
            end select
            close(unit)
        end block
    end subroutine debug_output_ast
    
    subroutine debug_output_codegen(input_file, code)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: code
        
        ! TODO: Implement codegen JSON output
        print *, "Debug codegen for ", trim(input_file), " (JSON output not implemented)"
    end subroutine debug_output_codegen

end module debug_utils