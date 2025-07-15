program debug_boundary_detection
    use frontend, only: is_end_function, is_function_start
    use lexer_core, only: tokenize_core, token_t
    implicit none
    
    character(len=*), parameter :: test_source = &
        "real function square(val)" // new_line('a') // &
        "  real :: val" // new_line('a') // &
        "  square = val * val" // new_line('a') // &
        "end function" // new_line('a') // &
        "" // new_line('a') // &
        "real function cube(val)" // new_line('a') // &
        "  real :: val" // new_line('a') // &
        "  cube = val * val * val" // new_line('a') // &
        "end function" // new_line('a')
    
    type(token_t), allocatable :: tokens(:)
    integer :: i
    
    ! Tokenize
    call tokenize_core(test_source, tokens)
    
    print *, "=== Token Analysis ==="
    do i = 1, size(tokens)
        print *, "Token", i, ":", trim(tokens(i)%text), &
                 " (line", tokens(i)%line, ")"
        
        if (is_function_start(tokens, i)) then
            print *, "  -> FUNCTION START detected"
        end if
        
        if (is_end_function(tokens, i)) then
            print *, "  -> END FUNCTION detected"
        end if
    end do
    
end program