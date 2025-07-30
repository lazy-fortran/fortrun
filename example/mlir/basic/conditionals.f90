program conditionals
    ! Demonstrate conditional statements
    integer :: x = 15
    integer :: y = 10
    logical :: condition

    ! Simple if statement
    if (x > y) then
        print *, "x is greater than y"
    end if

    ! If-else statement
    if (x < y) then
        print *, "x is less than y"
    else
        print *, "x is not less than y"
    end if

    ! If-elseif-else chain
    if (x == y) then
        print *, "x equals y"
    elseif (x > y) then
        print *, "x is greater than y"
    else
        print *, "x is less than y"
    end if

    ! Logical operations
    condition = (x > 0) .and. (y > 0)
    if (condition) then
        print *, "Both x and y are positive"
    end if

    condition = (x < 0) .or. (y < 0)
    if (condition) then
        print *, "At least one is negative"
    else
        print *, "Both are non-negative"
    end if

    ! Nested conditions
    if (x > 0) then
        if (x > 10) then
            print *, "x is greater than 10"
        else
            print *, "x is positive but not greater than 10"
        end if
    end if

end program conditionals
