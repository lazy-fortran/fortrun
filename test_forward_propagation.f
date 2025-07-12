! Test forward type propagation
y = square(5.0)

real(8) function square(x)
  square = x * x
end function