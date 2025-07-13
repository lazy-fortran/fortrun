module lexer
    ! Main lexer interface that delegates to appropriate dialect implementation
    use lexer_core
    use json_writer
    implicit none
    private
    
    ! Re-export token type and constants from core
    public :: token_t
    public :: TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING
    public :: TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, TK_UNKNOWN
    
    ! Main tokenization interface
    public :: tokenize
    
    ! Re-export JSON serialization functions
    public :: json_write_tokens_to_file, json_write_tokens_to_string
    
contains

    subroutine tokenize(source, tokens, dialect)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        character(len=*), intent(in), optional :: dialect
        
        character(len=20) :: active_dialect
        
        ! Determine which dialect to use
        if (present(dialect)) then
            active_dialect = dialect
        else
            ! Default to Simple Fortran for .f files
            active_dialect = "simple_fortran"
        end if
        
        ! For now, just use core tokenization
        ! TODO: Add dialect-specific tokenization
        call tokenize_core(source, tokens)
        
    end subroutine tokenize

end module lexer