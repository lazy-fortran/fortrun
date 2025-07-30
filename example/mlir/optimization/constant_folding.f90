program constant_folding
    ! Demonstrates constant folding optimization
    ! The compiler should evaluate constant expressions at compile time

    integer :: result1, result2, result3
    real :: real_result1, real_result2
    logical :: logical_result

    print *, "Constant Folding Optimization Example"
    print *, ""

    ! Simple arithmetic constants - should be folded to single constant
    result1 = 2 + 3*4  ! Should become: result1 = 14
    print *, "2 + 3 * 4 =", result1

    ! Nested arithmetic - should be completely evaluated
    result2 = (5 + 7)*(10 - 6)/2  ! Should become: result2 = 24
    print *, "(5 + 7) * (10 - 6) / 2 =", result2

    ! Power operations with constants
    result3 = 2**3 + 3**2  ! Should become: result3 = 17
    print *, "2^3 + 3^2 =", result3

    ! Floating point constants
    real_result1 = 3.14*2.0  ! Should become: real_result1 = 6.28
    print *, "3.14 * 2.0 =", real_result1

    ! Mathematical functions with constants
    real_result2 = sin(0.0) + cos(0.0)  ! Should become: real_result2 = 1.0
    print *, "sin(0) + cos(0) =", real_result2

    ! Logical constant expressions
    logical_result = .true. .and. .false.  ! Should become: logical_result = .false.
    print *, ".true. .and. .false. =", logical_result

    logical_result = .not. .false.  ! Should become: logical_result = .true.
    print *, ".not. .false. =", logical_result

    ! String concatenation with constants (if supported)
    ! character(len=20) :: str_result
    ! str_result = "Hello" // " " // "World"  ! Should become: str_result = "Hello World"

    print *, ""
    print *, "Expected MLIR optimizations:"
    print *, "- All arithmetic expressions folded to constants"
    print *, "- Function calls with constant args evaluated"
    print *, "- Logical expressions simplified"
    print *, "- No intermediate computations in final MLIR"

    ! Complex constant expression that should be completely folded
    call demonstrate_complex_folding()

contains

    subroutine demonstrate_complex_folding()
        integer, parameter :: a = 10
        integer, parameter :: b = 20
        integer, parameter :: c = 30
        integer :: complex_result

        print *, ""
        print *, "Complex constant folding:"

        ! This entire expression should be folded to a single constant
        ! because all operands are compile-time constants
        complex_result = ((a + b)*c)/(a - 5) + (b*2)
        ! Should become: complex_result = 220
        ! Calculation: ((10+20)*30)/(10-5) + (20*2) = (30*30)/5 + 40 = 900/5 + 40 = 180 + 40 = 220

        print *, "((a + b) * c) / (a - 5) + (b * 2) where a=10, b=20, c=30"
        print *, "Result:", complex_result
        print *, "Expected: 220"

        if (complex_result == 220) then
            print *, "Constant folding verification: PASSED"
        else
            print *, "Constant folding verification: FAILED"
        end if
    end subroutine demonstrate_complex_folding

end program constant_folding
