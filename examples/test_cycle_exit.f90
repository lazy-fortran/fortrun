program test_cycle_exit
    implicit none
    integer :: i
    
    ! Test simple cycle
    do i = 1, 10
        if (i == 5) cycle
        print *, "i = ", i
    end do
    
    print *, "---"
    
    ! Test simple exit
    do i = 1, 10
        if (i == 7) exit
        print *, "i = ", i
    end do
    
end program test_cycle_exit