program test_debug
    use lexer_core
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: source
    integer :: i
    
    source = "print *, 'hello' ! this is a comment"
    call tokenize_core(source, tokens)
    
    do i = 1, size(tokens)
        print '(a,i0,a,a,a,i0)', "Token ", i, ": '", trim(tokens(i)%text), "' kind=", tokens(i)%kind
    end do
end program test_debug
