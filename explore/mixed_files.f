! Test .f file with complex type inference scenarios
x = 42
y = 3.14159
name = "Fortran Test"
is_ready = .true.

! Arrays
numbers = [1, 2, 3, 4, 5]
reals = [1.0, 2.5, 3.7]

! Complex expressions  
result = x * y + sqrt(16.0)
complex_calc = sin(y) + cos(y) * numbers(1)

! String operations
full_name = trim(name) // " " // "Exploration"

print *, "Integer:", x
print *, "Real:", y  
print *, "String:", trim(full_name)
print *, "Logical:", is_ready
print *, "Array sum:", sum(numbers)
print *, "Complex result:", result
print *, "Trig result:", complex_calc