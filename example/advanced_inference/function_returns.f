! Function return type inference examples

! Simple function calls
area = compute_area(radius)
result = calculate_sum(x, y)
flag = is_valid(input_data)

! Intrinsic function calls  
angle = sin(x)
length = len_trim(text)
value = sqrt(number)

print *, 'Area:', area
print *, 'Result:', result  
print *, 'Flag:', flag
print *, 'Angle:', angle
print *, 'Length:', length
print *, 'Value:', value

! Note: Function definitions would be needed for user-defined functions
! The tool can infer types from intrinsic functions automatically