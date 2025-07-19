program debug_boundary
    use frontend, only: lex_file, parse_tokens
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    character(len=256) :: error_msg
    integer :: unit, i

    ! Simple if-else test case
    source = &
        "x = 1"//new_line('a')// &
        "if (x < 0) then"//new_line('a')// &
        "    print *, 'negative'"//new_line('a')// &
        "else if (x > 0) then"//new_line('a')// &
        "    print *, 'positive'"//new_line('a')// &
        "else"//new_line('a')// &
        "    print *, 'zero'"//new_line('a')// &
        "end if"//new_line('a')// &
        "print *, 'after if-else block'"

    ! Write to file and tokenize
    open (newunit=unit, file="/tmp/debug_boundary.f", status='replace')
    write (unit, '(A)') source
    close (unit)

    print *, "Tokenizing source from file..."
    call lex_file("/tmp/debug_boundary.f", tokens, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "ERROR:", trim(error_msg)
    else
        print *, "Total tokens:", size(tokens)
        print *, ""
        print *, "Tokens:"
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                print '("Token ", I3, ": EOF")', i
            else if (tokens(i)%kind == TK_KEYWORD) then
                print '("Token ", I3, ": [", A, "] (line ", I2, ")")', i, trim(tokens(i)%text), tokens(i)%line
            else
                print '("Token ", I3, ": ", A, " (line ", I2, ")")', i, trim(tokens(i)%text), tokens(i)%line
            end if
        end do
    end if

end program debug_boundary
