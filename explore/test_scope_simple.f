! Simple test to verify scope behavior
x = 5.0
result = double_it(x)
print *, "Result:", result

function double_it(input)
    implicit none
    ! Variables inside function
    factor = 2.0
    double_it = input * factor
end function