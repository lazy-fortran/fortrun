! Test intrinsic function type inference
x = 5.0
y = sqrt(x)
z = sin(x)
w = cos(x)
a = abs(-3.14)
b = max(x, y, z)
c = min(x, y, z)

print *, "x =", x
print *, "sqrt(x) =", y
print *, "sin(x) =", z  
print *, "cos(x) =", w
print *, "abs(-3.14) =", a
print *, "max =", b
print *, "min =", c