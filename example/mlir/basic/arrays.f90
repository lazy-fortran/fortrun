program arrays
    ! Demonstrate array declarations and operations
    integer, parameter :: n = 5
    integer :: arr(n) = [1, 2, 3, 4, 5]
    real :: matrix(3, 3)
    integer :: i, j
    integer :: sum_array = 0

    ! Print array elements
    print *, "Array elements:"
    do i = 1, n
        print *, "arr(", i, ") =", arr(i)
    end do

    ! Array sum
    do i = 1, n
        sum_array = sum_array + arr(i)
    end do
    print *, "Array sum:", sum_array

    ! Initialize 2D array
    do i = 1, 3
        do j = 1, 3
            matrix(i, j) = real(i*j)
        end do
    end do

    ! Print matrix
    print *, "Matrix:"
    do i = 1, 3
        print *, (matrix(i, j), j=1, 3)
    end do

    ! Array operations
    arr = arr*2  ! Multiply all elements by 2
    print *, "Array after multiplication by 2:"
    do i = 1, n
        print *, arr(i)
    end do

end program arrays
