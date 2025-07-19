program debug_manual_boundary
    use lexer_core, only: tokenize_core, token_t, TK_EOF, TK_KEYWORD
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    character(len=256) :: error_msg
    integer :: i, stmt_start, stmt_end

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

    print *, "Source:"
    print *, trim(source)
    print *, ""

    print *, "Tokenizing..."
    call tokenize_core(source, tokens)

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

    print *, ""
    print *, "Finding statement boundaries..."

    ! Manual call to find_statement_boundary for each statement
    i = 1
    do while (i <= size(tokens))
        if (tokens(i)%kind == TK_EOF) exit

        call find_statement_boundary(tokens, i, stmt_start, stmt_end)
    print '("Statement ", I2, ": tokens ", I3, " to ", I3, " (", A, " ... ", A, ")")', &
            (i-1)/10+1, stmt_start, stmt_end, trim(tokens(stmt_start)%text), trim(tokens(stmt_end)%text)

        i = stmt_end + 1
    end do

contains

    ! Copy of find_statement_boundary from frontend.f90
    subroutine find_statement_boundary(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end
        integer :: i, nesting_level
        logical :: in_if_block, in_do_loop, in_select_case

        stmt_start = start_pos
        stmt_end = start_pos
        in_if_block = .false.
        in_do_loop = .false.
        in_select_case = .false.
        nesting_level = 0

        ! Check for multi-line constructs
        if (is_if_then_start(tokens, start_pos)) then
            in_if_block = .true.
            nesting_level = 1
        end if

        if (nesting_level > 0) then
            ! Multi-line construct - find matching end
            i = start_pos
            do while (i <= size(tokens) .and. nesting_level > 0)
                if (tokens(i)%kind == TK_EOF) exit

                ! Check for end constructs
                if (in_if_block .and. is_end_if(tokens, i)) then
                    nesting_level = nesting_level - 1
                    if (nesting_level == 0) then
                        ! Check if it's "end if" (two tokens) or "endif" (one token)
                        if (i + 1 <= size(tokens) .and. tokens(i)%text == "end" .and. &
                 tokens(i + 1)%kind == TK_KEYWORD .and. tokens(i + 1)%text == "if") then
                            stmt_end = i + 1  ! Include both "end" and "if"
                        else
                            stmt_end = i  ! Just "endif"
                        end if
                        exit
                    end if
                end if

                i = i + 1
            end do

            if (stmt_end == start_pos) then
                stmt_end = i - 1  ! Couldn't find matching end
            end if
        else
            ! Single-line statement - find end of line
            i = start_pos
            do while (i <= size(tokens))
                if (tokens(i)%kind == TK_EOF) then
                    stmt_end = i - 1
                    exit
               else if (i < size(tokens) .and. tokens(i)%line < tokens(i + 1)%line) then
                    stmt_end = i
                    exit
                else
                    stmt_end = i
                    i = i + 1
                end if
            end do
        end if
    end subroutine find_statement_boundary

    logical function is_if_then_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        is_if_then_start = .false.
        if (pos > size(tokens)) return

        ! Check if current token is "if"
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "if") then
            ! Look for "then" on the same line
            i = pos + 1
            do while (i <= size(tokens) .and. tokens(i)%line == tokens(pos)%line)
                if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "then") then
                    is_if_then_start = .true.
                    exit
                end if
                i = i + 1
            end do
        end if
    end function is_if_then_start

    logical function is_end_if(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_if = .false.
        if (pos > size(tokens)) return

        ! Check for "endif" (single keyword)
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "endif") then
            is_end_if = .true.
            return
        end if

        ! Check for "end if" (two keywords)
        if (pos + 1 <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
             tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "if") then
                is_end_if = .true.
            end if
        end if
    end function is_end_if

end program debug_manual_boundary
