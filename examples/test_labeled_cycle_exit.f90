program test_labeled_cycle_exit
    implicit none
    integer :: i, j
    
    ! Test labeled loops with cycle and exit
    outer: do i = 1, 3
        print *, "Outer loop i = ", i
        inner: do j = 1, 4
            if (i == 2 .and. j == 2) then
                print *, "  Cycling inner loop at j = ", j
                cycle inner
            end if
            if (i == 3 .and. j == 3) then
                print *, "  Exiting outer loop from inner at j = ", j
                exit outer
            end if
            print *, "  Inner loop j = ", j
        end do inner
    end do outer
    
    print *, "Done!"
end program test_labeled_cycle_exit