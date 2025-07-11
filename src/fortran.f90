module fortran
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fortran!"
  end subroutine say_hello
end module fortran
