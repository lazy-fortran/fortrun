program math_functions
    implicit none
    real(8) :: x, result
    
    x = 4.0_8
    
    result = sqrt(x)
    print *, 'sqrt(4) =', result
    
    result = sin(x)
    print *, 'sin(4) =', result
    
    result = exp(x)
    print *, 'exp(4) =', result
    
    result = abs(-x)
    print *, 'abs(-4) =', result
end program math_functions