program dead_code_elimination
    ! Demonstrates dead code elimination optimization
    ! The compiler should remove code that never executes or whose results are never used

    integer :: x = 10
    integer :: y = 20
    integer :: z, unused_var
    logical :: condition = .false.

    print *, "Dead Code Elimination Example"
    print *, ""

    ! Used computation - should be kept
    z = x + y
    print *, "Used result: z =", z

    ! Dead assignment - result never used, should be eliminated
    unused_var = 42*17 + 100  ! This should be removed

    ! Dead conditional block - condition is always false
    if (.false.) then
        ! This entire block should be eliminated
        print *, "This will never execute"
        x = x*1000
        y = y + 999
        z = x + y + 123456
    end if

    ! Dead code after unconditional branch
    if (.true.) then
        print *, "This executes"
    else
        ! This else block should be eliminated (unreachable)
        print *, "This will never execute either"
        x = -999
    end if

    ! Variable defined but never used after this point - should be eliminated
    unused_var = x + y  ! This assignment should be removed

    ! More complex dead code pattern
    call demonstrate_dead_function_calls()

    ! Final used computation
    z = x*2
    print *, "Final result:", z

    print *, ""
    print *, "Expected MLIR optimizations:"
    print *, "- Unused variable assignments removed"
    print *, "- Unreachable if/else blocks eliminated"
    print *, "- Dead function calls removed"
    print *, "- Unused intermediate computations eliminated"

contains

    subroutine demonstrate_dead_function_calls()
        integer :: temp1, temp2, temp3
        logical :: always_false = .false.

        print *, ""
        print *, "Dead function call elimination:"

        ! Function call whose result is used - should be kept
        temp1 = useful_function(5)
        print *, "Used function result:", temp1

        ! Function call whose result is never used - should be eliminated
        ! (assuming the function has no side effects)
        temp2 = pure_function(10)  ! This call should be removed

        ! Function call in dead conditional - should be eliminated
        if (always_false) then
            temp3 = expensive_function(100)  ! This should be removed
            print *, "Dead code:", temp3
        end if

        ! Dead loop - never executes
        do while (.false.)
            temp3 = expensive_function(200)  ! This should be removed
            print *, "Dead loop:", temp3
        end do
    end subroutine demonstrate_dead_function_calls

    integer function useful_function(n)
        integer, intent(in) :: n
        useful_function = n*n + 1
    end function useful_function

    integer function pure_function(n)
        ! Pure function with no side effects
        integer, intent(in) :: n
        pure_function = n + 42
    end function pure_function

    integer function expensive_function(n)
        ! Simulates an expensive computation
        integer, intent(in) :: n
        integer :: i, result = 0

        do i = 1, n
            result = result + i*i
        end do
        expensive_function = result
    end function expensive_function

end program dead_code_elimination
