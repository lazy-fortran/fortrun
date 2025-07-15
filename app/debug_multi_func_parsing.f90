program debug_multi_func_parsing
    use frontend, only: find_program_unit_boundary, is_function_start
    use lexer_core, only: tokenize_core, token_t
    implicit none
    
    character(len=*), parameter :: test_source = &
        "x = 5.0" // new_line('a') // &
        "y = square(x)" // new_line('a') // &
        "" // new_line('a') // &
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
    integer :: i, unit_start, unit_end, unit_count
    
    ! Tokenize
    call tokenize_core(test_source, tokens)
    
    print *, "Total tokens:", size(tokens)
    
    ! Parse program units
    unit_count = 0
    i = 1
    do while (i <= size(tokens))
        if (tokens(i)%kind == 6) exit  ! TK_EOF
        
        call find_program_unit_boundary(tokens, i, unit_start, unit_end)
        unit_count = unit_count + 1
        
        print *, "Unit", unit_count, ": tokens", unit_start, "to", unit_end
        print *, "  Start token:", trim(tokens(unit_start)%text)
        print *, "  End token:", trim(tokens(unit_end)%text)
        print *, "  Is function start:", is_function_start(tokens, unit_start)
        
        i = unit_end + 1
    end do
    
    print *, "Total units found:", unit_count
end program