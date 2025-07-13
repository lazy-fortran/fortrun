program test_lexer_basic
    use lexer, only: token, tokenize, TK_IDENTIFIER, TK_OPERATOR, &
                    TK_NUMBER, TK_STRING, TK_KEYWORD, TK_NEWLINE, TK_EOF
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_simple_assignment(test_count, pass_count)
    call test_print_statement(test_count, pass_count)
    call test_arithmetic_expression(test_count, pass_count)
    call test_token_positions(test_count, pass_count)

    print '(a,i0,a,i0)', 'Lexer basic tests: ', pass_count, '/', test_count, ' passed'
    if (pass_count /= test_count) stop 1

contains

    subroutine test_simple_assignment(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        type(token), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        source = "x = 5.0"
        call tokenize(source, tokens)
        
        test_count = test_count + 1
        if (size(tokens) >= 4) then
            if (tokens(1)%kind == TK_IDENTIFIER .and. &
                tokens(1)%text == "x" .and. &
                tokens(2)%kind == TK_OPERATOR .and. &
                tokens(2)%text == "=" .and. &
                tokens(3)%kind == TK_NUMBER .and. &
                tokens(3)%text == "5.0" .and. &
                tokens(4)%kind == TK_EOF) then
                pass_count = pass_count + 1
                print '(a)', 'PASS: Simple assignment tokenization'
            else
                print '(a)', 'FAIL: Simple assignment - incorrect tokens'
                call print_tokens(tokens)
            end if
        else
            print '(a,i0)', 'FAIL: Simple assignment - wrong token count: ', size(tokens)
        end if
    end subroutine test_simple_assignment

    subroutine test_print_statement(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        type(token), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        source = 'print *, "Hello World"'
        call tokenize(source, tokens)
        
        test_count = test_count + 1
        if (size(tokens) >= 5) then
            if (tokens(1)%kind == TK_KEYWORD .and. &
                tokens(1)%text == "print" .and. &
                tokens(2)%kind == TK_OPERATOR .and. &
                tokens(2)%text == "*" .and. &
                tokens(3)%kind == TK_OPERATOR .and. &
                tokens(3)%text == "," .and. &
                tokens(4)%kind == TK_STRING .and. &
                tokens(4)%text == '"Hello World"' .and. &
                tokens(5)%kind == TK_EOF) then
                pass_count = pass_count + 1
                print '(a)', 'PASS: Print statement tokenization'
            else
                print '(a)', 'FAIL: Print statement - incorrect tokens'
                call print_tokens(tokens)
            end if
        else
            print '(a,i0)', 'FAIL: Print statement - wrong token count: ', size(tokens)
        end if
    end subroutine test_print_statement

    subroutine test_arithmetic_expression(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        type(token), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        source = "result = 2.0 + 3.0 * x"
        call tokenize(source, tokens)
        
        test_count = test_count + 1
        if (size(tokens) >= 8) then
            if (tokens(1)%kind == TK_IDENTIFIER .and. tokens(1)%text == "result" .and. &
                tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == "=" .and. &
                tokens(3)%kind == TK_NUMBER .and. tokens(3)%text == "2.0" .and. &
                tokens(4)%kind == TK_OPERATOR .and. tokens(4)%text == "+" .and. &
                tokens(5)%kind == TK_NUMBER .and. tokens(5)%text == "3.0" .and. &
                tokens(6)%kind == TK_OPERATOR .and. tokens(6)%text == "*" .and. &
                tokens(7)%kind == TK_IDENTIFIER .and. tokens(7)%text == "x" .and. &
                tokens(8)%kind == TK_EOF) then
                pass_count = pass_count + 1
                print '(a)', 'PASS: Arithmetic expression tokenization'
            else
                print '(a)', 'FAIL: Arithmetic expression - incorrect tokens'
                call print_tokens(tokens)
            end if
        else
            print '(a,i0)', 'FAIL: Arithmetic expression - wrong token count: ', size(tokens)
        end if
    end subroutine test_arithmetic_expression

    subroutine test_token_positions(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        type(token), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        source = "x = 5"
        call tokenize(source, tokens)
        
        test_count = test_count + 1
        if (size(tokens) >= 3) then
            ! Check positions: x at column 1, = at column 3, 5 at column 5
            if (tokens(1)%column == 1 .and. &
                tokens(2)%column == 3 .and. &
                tokens(3)%column == 5 .and. &
                tokens(1)%line == 1 .and. &
                tokens(2)%line == 1 .and. &
                tokens(3)%line == 1) then
                pass_count = pass_count + 1
                print '(a)', 'PASS: Token position tracking'
            else
                print '(a)', 'FAIL: Token positions incorrect'
                print '(a,i0,a,i0)', '  Token 1: line ', tokens(1)%line, ', col ', tokens(1)%column
                print '(a,i0,a,i0)', '  Token 2: line ', tokens(2)%line, ', col ', tokens(2)%column
                print '(a,i0,a,i0)', '  Token 3: line ', tokens(3)%line, ', col ', tokens(3)%column
            end if
        else
            print '(a)', 'FAIL: Token positions - insufficient tokens'
        end if
    end subroutine test_token_positions

    subroutine print_tokens(tokens)
        type(token), intent(in) :: tokens(:)
        integer :: i
        
        do i = 1, size(tokens)
            print '(a,i0,a,i0,a,a,a)', '  Token ', i, ' (kind=', tokens(i)%kind, &
                  '): "', tokens(i)%text, '"'
        end do
    end subroutine print_tokens

end program test_lexer_basic