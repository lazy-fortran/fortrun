x = 5.0
result = square(x)
print *, "Result:", result

real function square(a)
    real :: a
    square = a * a
end function square