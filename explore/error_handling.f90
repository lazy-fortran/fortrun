! Test error handling scenarios
program error_test
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    
    integer :: status
    real :: x, y, result
    
    ! Division by zero test
    x = 10.0
    y = 0.0
    
    if (y == 0.0) then
        write(error_unit, *) "Warning: Division by zero detected"
        result = huge(result)
    else
        result = x / y
    end if
    
    print *, "x =", x
    print *, "y =", y  
    print *, "result =", result
    
    ! File operation error test
    open(unit=99, file="nonexistent_file.txt", status="old", iostat=status)
    if (status /= 0) then
        write(error_unit, *) "Info: File not found (expected for this test)"
    else
        close(99)
    end if
    
end program