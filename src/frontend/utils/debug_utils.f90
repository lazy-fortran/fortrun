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

end module debug_utils