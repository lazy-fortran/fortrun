program test_lexer_errors
    use lexer
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run error handling and edge case tests
    if (.not. test_empty_input()) all_passed = .false.
    if (.not. test_unterminated_strings()) all_passed = .false.
    if (.not. test_unknown_characters()) all_passed = .false.
    if (.not. test_position_tracking()) all_passed = .false.
    if (.not. test_mixed_content()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All lexer error handling tests passed"
        stop 0
    else
        print '(a)', "Some lexer error handling tests failed"
        stop 1
    end if

contains

    logical function test_empty_input()
        type(token), allocatable :: tokens(:)
        
        test_empty_input = .true.
        print '(a)', "Testing empty input handling..."
        
        ! Test completely empty string
        call tokenize("", tokens)
        if (size(tokens) /= 1) then
            print '(a)', "FAIL: Empty input should produce only EOF token"
            test_empty_input = .false.
            return
        end if
        
        if (tokens(1)%kind /= TK_EOF) then
            print '(a)', "FAIL: Empty input should produce EOF token"
            test_empty_input = .false.
            return
        end if
        
        ! Test whitespace only
        call tokenize("   ", tokens)
        if (size(tokens) /= 1 .or. tokens(1)%kind /= TK_EOF) then
            print '(a)', "FAIL: Whitespace-only input should produce only EOF token"
            test_empty_input = .false.
            return
        end if
        
        print '(a)', "PASS: Empty input handling"
    end function test_empty_input

    logical function test_unterminated_strings()
        type(token), allocatable :: tokens(:)
        
        test_unterminated_strings = .true.
        print '(a)', "Testing unterminated string handling..."
        
        ! Test unterminated double quote string
        call tokenize('"hello', tokens)
        if (size(tokens) /= 2) then  ! string + EOF
            print '(a)', "FAIL: Wrong token count for unterminated string"
            test_unterminated_strings = .false.
            return
        end if
        
        if (tokens(1)%kind /= TK_STRING) then
            print '(a)', "FAIL: Unterminated string should still be STRING token"
            test_unterminated_strings = .false.
            return
        end if
        
        if (tokens(1)%text /= '"hello') then
            print '(a)', "FAIL: Unterminated string text incorrect"
            test_unterminated_strings = .false.
            return
        end if
        
        ! Test unterminated single quote string
        call tokenize("'world", tokens)
        if (tokens(1)%kind /= TK_STRING .or. tokens(1)%text /= "'world") then
            print '(a)', "FAIL: Unterminated single quote string"
            test_unterminated_strings = .false.
            return
        end if
        
        print '(a)', "PASS: Unterminated string handling"
    end function test_unterminated_strings

    logical function test_unknown_characters()
        type(token), allocatable :: tokens(:)
        
        test_unknown_characters = .true.
        print '(a)', "Testing unknown character handling..."
        
        ! Test characters not in our lexer's character set
        call tokenize("@", tokens)
        if (size(tokens) /= 1) then  ! Only EOF since @ is skipped
            print '(a)', "FAIL: Unknown character should be skipped"
            test_unknown_characters = .false.
            return
        end if
        
        if (tokens(1)%kind /= TK_EOF) then
            print '(a)', "FAIL: Unknown character should result in EOF only"
            test_unknown_characters = .false.
            return
        end if
        
        ! Test hash character
        call tokenize("#", tokens)
        if (size(tokens) /= 1 .or. tokens(1)%kind /= TK_EOF) then
            print '(a)', "FAIL: Hash character should be skipped"
            test_unknown_characters = .false.
            return
        end if
        
        print '(a)', "PASS: Unknown character handling"
    end function test_unknown_characters

    logical function test_position_tracking()
        type(token), allocatable :: tokens(:)
        
        test_position_tracking = .true.
        print '(a)', "Testing position tracking..."
        
        ! Test single line positions
        call tokenize("x = 42", tokens)
        if (size(tokens) /= 4) then  ! identifier, operator, number, EOF
            print '(a)', "FAIL: Wrong token count for position test"
            test_position_tracking = .false.
            return
        end if
        
        ! Check positions
        if (tokens(1)%line /= 1 .or. tokens(1)%column /= 1) then
            print '(a)', "FAIL: First token position incorrect"
            test_position_tracking = .false.
            return
        end if
        
        if (tokens(2)%line /= 1 .or. tokens(2)%column /= 3) then
            print '(a)', "FAIL: Second token position incorrect"
            test_position_tracking = .false.
            return
        end if
        
        if (tokens(3)%line /= 1 .or. tokens(3)%column /= 5) then
            print '(a)', "FAIL: Third token position incorrect"
            test_position_tracking = .false.
            return
        end if
        
        print '(a)', "PASS: Position tracking"
    end function test_position_tracking

    logical function test_mixed_content()
        type(token), allocatable :: tokens(:)
        
        test_mixed_content = .true.
        print '(a)', "Testing mixed content tokenization..."
        
        ! Test complex expression with multiple token types
        call tokenize('print "hello" + 123', tokens)
        
        ! Expected: print(keyword), "hello"(string), +(operator), 123(number), EOF
        if (size(tokens) /= 5) then
            print '(a,i0)', "FAIL: Wrong token count for mixed content, got: ", size(tokens)
            test_mixed_content = .false.
            return
        end if
        
        ! Check token types
        if (tokens(1)%kind /= TK_KEYWORD .or. tokens(1)%text /= "print") then
            print '(a)', "FAIL: First token should be print keyword"
            test_mixed_content = .false.
            return
        end if
        
        if (tokens(2)%kind /= TK_STRING .or. tokens(2)%text /= '"hello"') then
            print '(a)', "FAIL: Second token should be string literal"
            test_mixed_content = .false.
            return
        end if
        
        if (tokens(3)%kind /= TK_OPERATOR .or. tokens(3)%text /= "+") then
            print '(a)', "FAIL: Third token should be + operator"
            test_mixed_content = .false.
            return
        end if
        
        if (tokens(4)%kind /= TK_NUMBER .or. tokens(4)%text /= "123") then
            print '(a)', "FAIL: Fourth token should be number literal"
            test_mixed_content = .false.
            return
        end if
        
        if (tokens(5)%kind /= TK_EOF) then
            print '(a)', "FAIL: Last token should be EOF"
            test_mixed_content = .false.
            return
        end if
        
        print '(a)', "PASS: Mixed content tokenization"
    end function test_mixed_content

end program test_lexer_errors