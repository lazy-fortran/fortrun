module simple_module
  implicit none
  public :: simple_function
contains
  function simple_function() result(x)
    integer :: x
    x = 42
  end function simple_function
end module simple_module