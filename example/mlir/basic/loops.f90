program loops
    ! Demonstrate loop constructs
    integer :: i
    integer :: sum = 0
    integer :: factorial = 1

    ! Simple do loop
    print *, "Counting from 1 to 5:"
    do i = 1, 5
        print *, i
    end do

    ! Loop with accumulation
    print *, "Computing sum of 1 to 10:"
    do i = 1, 10
        sum = sum + i
    end do
    print *, "Sum:", sum

    ! Loop with step
    print *, "Even numbers from 2 to 10:"
    do i = 2, 10, 2
        print *, i
    end do

    ! Factorial computation
    print *, "Computing 5! factorial:"
    do i = 1, 5
        factorial = factorial*i
    end do
    print *, "5! =", factorial

end program loops
