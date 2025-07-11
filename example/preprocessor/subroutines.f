! Example with subroutines
real :: radius, area, circumference

radius = 5.0
call calculate_circle(radius, area, circumference)

print *, 'Radius:', radius
print *, 'Area:', area
print *, 'Circumference:', circumference

subroutine calculate_circle(r, a, c)
  real :: r, a, c
  real :: pi = 3.14159265359
  
  a = pi * r * r
  c = 2.0 * pi * r
end subroutine calculate_circle