! Test simple function without parameters
x = 5.0
y = square(x)
print *, y

function square(val)
    square = val * val
end function