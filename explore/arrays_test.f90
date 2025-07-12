! Test array handling in .f90 for comparison
program arrays_test
    implicit none
    
    integer, parameter :: n = 5
    integer :: numbers(n) = [1, 2, 3, 4, 5]
    integer :: total
    real :: average
    
    total = sum(numbers)
    average = real(total) / real(size(numbers))

    print *, "Numbers:", numbers
    print *, "Sum:", total
    print *, "Average:", average
end program