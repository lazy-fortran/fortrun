module debug_state
    implicit none
    private
    
    public :: set_debug_flags, get_debug_flags
    
    ! Global debug state
    logical, save :: debug_tokens_flag = .false.
    logical, save :: debug_ast_flag = .false.
    logical, save :: debug_codegen_flag = .false.
    
contains

    subroutine set_debug_flags(tokens, ast, codegen)
        logical, intent(in) :: tokens, ast, codegen
        debug_tokens_flag = tokens
        debug_ast_flag = ast
        debug_codegen_flag = codegen
    end subroutine set_debug_flags
    
    subroutine get_debug_flags(tokens, ast, codegen)
        logical, intent(out) :: tokens, ast, codegen
        tokens = debug_tokens_flag
        ast = debug_ast_flag
        codegen = debug_codegen_flag
    end subroutine get_debug_flags

end module debug_state