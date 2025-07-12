! all_types.f90 - Standard Fortran with explicit type declarations
program all_types
  implicit none
  integer :: count, sum_int
  real(8) :: pi, product_real, mixed
  character(len=7) :: name
  logical :: ready, is_positive
  
  count = 42
  pi = 3.14159_8
  name = "Fortran"
  ready = .true.

  ! Arithmetic operations preserve types
  sum_int = count + 10
  product_real = pi * 2.0_8

  ! Mixed expressions promote to real
  mixed = real(count, 8) + pi

  ! Logical from comparison
  is_positive = count > 0

  ! Print all values
  print *, 'Integer count:', count
  print *, 'Real pi:', pi
  print *, 'String name:', name
  print *, 'Logical ready:', ready
  print *, 'Integer sum:', sum_int
  print *, 'Real product:', product_real
  print *, 'Mixed result:', mixed
  print *, 'Is positive?', is_positive
end program all_types