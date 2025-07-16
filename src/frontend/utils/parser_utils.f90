module parser_utils
    ! Parser utility functions for text processing and navigation
    
    use parser_core, only: parser_state_t
    implicit none
    private
    
    ! Public interface
    public :: remove_inline_comments
    public :: advance_to_next_statement
    
contains

    ! Remove inline comments from a line, respecting string literals
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

end module parser_utils