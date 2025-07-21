! # Simple Mathematical Computations
! This notebook demonstrates basic mathematical operations in Fortran

! ## Basic Arithmetic Operations
! Let's start with simple arithmetic

x = 10
y = 20
sum = x + y
product = x*y

print *, "x =", x
print *, "y =", y
print *, "Sum:", sum
print *, "Product:", product

! ## Working with Arrays
! Now let's work with arrays

integer :: numbers(5)
numbers = [1, 2, 3, 4, 5]

total = sum(numbers)
average = real(total)/size(numbers)

print *, "Numbers:", numbers
print *, "Total:", total
print *, "Average:", average

! ## Mathematical Functions
! Using built-in mathematical functions

angle = 3.14159/4.0  ! 45 degrees in radians
sine_val = sin(angle)
cosine_val = cos(angle)

print *, "Angle (radians):", angle
print *, "Sin:", sine_val
print *, "Cos:", cosine_val
