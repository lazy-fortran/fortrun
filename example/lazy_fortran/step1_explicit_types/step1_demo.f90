! Step 1: Generated output from step1_demo.f
! This shows the opinionated defaults applied by the preprocessor

program main
    implicit none

    ! Auto-generated variable declarations:
    real(8) :: result

    ! Step 1: Explicit function types with opinionated defaults
    ! This demonstrates how the preprocessor applies modern defaults to explicit types

    result = square(5.0_8)
    print *, "Square of 5.0 is:", result

contains
    real(8) function square(x)  ! Enhanced: real -> real(8)
        implicit none

        ! Auto-generated variable declarations:
        real(8), intent(in) :: x  ! Enhanced: real :: x -> real(8), intent(in) :: x

        square = x*x
    end function
end program main
