! Test array handling in .f files
numbers = [1, 2, 3, 4, 5]
total = sum(numbers)
average = total / size(numbers)

print *, "Numbers:", numbers
print *, "Sum:", total
print *, "Average:", average