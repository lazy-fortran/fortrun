! Test function definitions in .f files
x = 5.0
y = square(x)
z = cube(x)

print *, "x =", x
print *, "x^2 =", y  
print *, "x^3 =", z

function square(val)
    result = val * val
    square = result
end function

function cube(val)
    result = val * val * val
    cube = result
end function