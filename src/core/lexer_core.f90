module lexer_core
    implicit none
    private

    ! Token types
    integer, parameter, public :: TK_EOF = 0
    integer, parameter, public :: TK_IDENTIFIER = 1
    integer, parameter, public :: TK_NUMBER = 2
    integer, parameter, public :: TK_STRING = 3
    integer, parameter, public :: TK_OPERATOR = 4
    integer, parameter, public :: TK_KEYWORD = 5
    integer, parameter, public :: TK_NEWLINE = 6
    integer, parameter, public :: TK_COMMENT = 7
    integer, parameter, public :: TK_UNKNOWN = 99

    ! Token structure
    type, public :: token
        integer :: kind = TK_UNKNOWN
        character(len=:), allocatable :: text
        integer :: line = 1
        integer :: column = 1
    end type token

    ! Public interface
    public :: tokenize_core

    ! Keywords list
    character(len=20), dimension(20) :: keywords = [ &
        "program     ", "end         ", "function    ", "subroutine  ", &
        "if          ", "then        ", "else        ", "endif       ", &
        "do          ", "while       ", "implicit    ", "none        ", &
        "integer     ", "real        ", "logical     ", "character   ", &
        "print       ", "read        ", "write       ", "call        " &
    ]

contains

    subroutine tokenize_core(source, tokens)
        character(len=*), intent(in) :: source
        type(token), allocatable, intent(out) :: tokens(:)
        
        type(token), allocatable :: temp_tokens(:)
        integer :: pos, line_num, col_num, token_count
        integer :: source_len
        character(len=1) :: ch
        
        ! Initialize
        source_len = len(source)
        pos = 1
        line_num = 1
        col_num = 1
        token_count = 0
        allocate(temp_tokens(100))  ! Initial allocation
        
        ! Main tokenization loop
        do while (pos <= source_len)
            ch = source(pos:pos)
            
            ! Skip whitespace
            if (is_whitespace(ch)) then
                if (ch == new_line('a')) then
                    line_num = line_num + 1
                    col_num = 1
                else
                    col_num = col_num + 1
                end if
                pos = pos + 1
                cycle
            end if
            
            ! Number literal
            if (is_digit(ch)) then
                call scan_number(source, pos, line_num, col_num, temp_tokens, token_count)
            
            ! String literal
            else if (ch == '"' .or. ch == "'") then
                call scan_string(source, pos, line_num, col_num, temp_tokens, token_count)
            
            ! Identifier or keyword
            else if (is_letter(ch)) then
                call scan_identifier(source, pos, line_num, col_num, temp_tokens, token_count)
            
            ! Operators
            else if (is_operator(ch)) then
                call scan_operator(source, pos, line_num, col_num, temp_tokens, token_count)
            
            ! Unknown character
            else
                pos = pos + 1
                col_num = col_num + 1
            end if
        end do
        
        ! Add EOF token
        token_count = token_count + 1
        if (token_count > size(temp_tokens)) then
            call resize_tokens(temp_tokens)
        end if
        temp_tokens(token_count)%kind = TK_EOF
        temp_tokens(token_count)%text = ""
        temp_tokens(token_count)%line = line_num
        temp_tokens(token_count)%column = col_num
        
        ! Copy to output array
        allocate(tokens(token_count))
        tokens = temp_tokens(1:token_count)
        
    end subroutine tokenize_core

    subroutine scan_number(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token), allocatable, intent(inout) :: tokens(:)
        
        integer :: start_pos, start_col
        logical :: has_dot
        
        start_pos = pos
        start_col = col_num
        has_dot = .false.
        
        ! Scan integer part
        do while (pos <= len(source))
            if (.not. is_digit(source(pos:pos))) exit
            pos = pos + 1
            col_num = col_num + 1
        end do
        
        ! Check for decimal point
        if (pos <= len(source)) then
            if (source(pos:pos) == '.') then
                has_dot = .true.
                pos = pos + 1
                col_num = col_num + 1
                
                ! Scan fractional part
                do while (pos <= len(source))
                    if (.not. is_digit(source(pos:pos))) exit
                    pos = pos + 1
                    col_num = col_num + 1
                end do
            end if
        end if
        
        ! Check for exponent
        if (pos <= len(source)) then
            if (source(pos:pos) == 'e' .or. source(pos:pos) == 'E' .or. &
                source(pos:pos) == 'd' .or. source(pos:pos) == 'D') then
                pos = pos + 1
                col_num = col_num + 1
                
                ! Optional sign
                if (pos <= len(source)) then
                    if (source(pos:pos) == '+' .or. source(pos:pos) == '-') then
                        pos = pos + 1
                        col_num = col_num + 1
                    end if
                end if
                
                ! Exponent digits
                do while (pos <= len(source))
                    if (.not. is_digit(source(pos:pos))) exit
                    pos = pos + 1
                    col_num = col_num + 1
                end do
            end if
        end if
        
        ! Add token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_NUMBER
        tokens(token_count)%text = source(start_pos:pos-1)
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col
        
    end subroutine scan_number

    subroutine scan_string(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token), allocatable, intent(inout) :: tokens(:)
        
        integer :: start_pos, start_col
        character(len=1) :: quote_char
        
        start_pos = pos
        start_col = col_num
        quote_char = source(pos:pos)
        
        pos = pos + 1
        col_num = col_num + 1
        
        ! Scan until closing quote
        do while (pos <= len(source))
            if (source(pos:pos) == quote_char) then
                pos = pos + 1
                col_num = col_num + 1
                exit
            else if (source(pos:pos) == new_line('a')) then
                ! Error: unterminated string
                exit
            else
                pos = pos + 1
                col_num = col_num + 1
            end if
        end do
        
        ! Add token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_STRING
        tokens(token_count)%text = source(start_pos:pos-1)
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col
        
    end subroutine scan_string

    subroutine scan_identifier(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token), allocatable, intent(inout) :: tokens(:)
        
        integer :: start_pos, start_col
        character(len=:), allocatable :: word
        
        start_pos = pos
        start_col = col_num
        
        ! Scan identifier
        do while (pos <= len(source))
            if (.not. (is_letter(source(pos:pos)) .or. &
                       is_digit(source(pos:pos)) .or. &
                       source(pos:pos) == '_')) exit
            pos = pos + 1
            col_num = col_num + 1
        end do
        
        word = source(start_pos:pos-1)
        
        ! Add token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        
        ! Check if it's a keyword
        if (is_keyword(word)) then
            tokens(token_count)%kind = TK_KEYWORD
        else
            tokens(token_count)%kind = TK_IDENTIFIER
        end if
        
        tokens(token_count)%text = word
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col
        
    end subroutine scan_identifier

    subroutine scan_operator(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token), allocatable, intent(inout) :: tokens(:)
        
        integer :: start_col
        character(len=2) :: two_char
        
        start_col = col_num
        
        ! Check for two-character operators
        if (pos < len(source)) then
            two_char = source(pos:pos+1)
            if (two_char == "==" .or. two_char == "/=" .or. &
                two_char == "<=" .or. two_char == ">=" .or. &
                two_char == "::" .or. two_char == "**") then
                
                token_count = token_count + 1
                if (token_count > size(tokens)) then
                    call resize_tokens(tokens)
                end if
                tokens(token_count)%kind = TK_OPERATOR
                tokens(token_count)%text = two_char
                tokens(token_count)%line = line_num
                tokens(token_count)%column = start_col
                
                pos = pos + 2
                col_num = col_num + 2
                return
            end if
        end if
        
        ! Single character operator
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_OPERATOR
        tokens(token_count)%text = source(pos:pos)
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col
        
        pos = pos + 1
        col_num = col_num + 1
        
    end subroutine scan_operator

    ! Helper functions
    
    logical function is_whitespace(ch)
        character(len=1), intent(in) :: ch
        is_whitespace = (ch == ' ' .or. ch == char(9) .or. ch == new_line('a'))
    end function is_whitespace

    logical function is_letter(ch)
        character(len=1), intent(in) :: ch
        is_letter = (ch >= 'A' .and. ch <= 'Z') .or. (ch >= 'a' .and. ch <= 'z')
    end function is_letter

    logical function is_digit(ch)
        character(len=1), intent(in) :: ch
        is_digit = (ch >= '0' .and. ch <= '9')
    end function is_digit

    logical function is_operator(ch)
        character(len=1), intent(in) :: ch
        is_operator = index("+-*/=<>()[]{},:;.", ch) > 0
    end function is_operator

    logical function is_keyword(word)
        character(len=*), intent(in) :: word
        integer :: i
        character(len=20) :: lower_word
        
        ! Convert to lowercase for comparison
        lower_word = to_lower(word)
        
        do i = 1, size(keywords)
            if (trim(lower_word) == trim(keywords(i))) then
                is_keyword = .true.
                return
            end if
        end do
        
        is_keyword = .false.
    end function is_keyword

    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i
        
        do i = 1, len(str)
            if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') then
                lower_str(i:i) = char(ichar(str(i:i)) + 32)
            else
                lower_str(i:i) = str(i:i)
            end if
        end do
    end function to_lower

    subroutine resize_tokens(tokens)
        type(token), allocatable, intent(inout) :: tokens(:)
        type(token), allocatable :: temp(:)
        
        allocate(temp(size(tokens) * 2))
        temp(1:size(tokens)) = tokens
        call move_alloc(temp, tokens)
    end subroutine resize_tokens

end module lexer_core