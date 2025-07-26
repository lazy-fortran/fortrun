program test_array_literal
    implicit none
    integer :: arr(3)
    
    arr = [10, 20, 30]
    
    print *, "Array values:", arr
end program test_array_literal