program simple_test
    implicit none
    real :: x, y
    integer :: i
    
    x = 5.0
    y = x * 2.0 + 1.0
    
    print *, 'Simple calculation result:', y
    
    do i = 1, 3
        print *, 'Loop iteration:', i
    end do
end program simple_test