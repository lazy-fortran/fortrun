module math_utils
  private
  public :: add, multiply
  
contains

  function add(a, b) result(c)
    c = a + b
  end function add
  
  function multiply(a, b) result(c)
    c = a * b
  end function multiply
  
end module math_utils