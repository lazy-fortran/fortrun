! Test if type inference works inside functions with the partial fix
x = 5.0
result = double_it(x)
print *, "Result:", result

function double_it(input)
    ! Variables inside function
    factor = 2.0
    double_it = input * factor
end function