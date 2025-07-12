! Step 1: Working explicit types example
result = compute(5.0)
print *, "Result:", result

real function compute(x)
    real :: x
    compute = x * x + 1.0
end function