program algebraic_simplification
    ! Demonstrates algebraic simplification optimizations
    ! The compiler should recognize and simplify algebraic patterns

    real :: x = 7.0
    real :: y = 3.0
    real :: result
    integer :: int_x = 5
    integer :: int_y = 2
    integer :: int_result

    print *, "Algebraic Simplification Example"
    print *, ""
    print *, "Input values: x =", x, ", y =", y
    print *, ""

    ! Identity operations - should be simplified
    result = x + 0.0  ! Should become: result = x
    print *, "x + 0 =", result

    result = x*1.0  ! Should become: result = x
    print *, "x * 1 =", result

    result = x - 0.0  ! Should become: result = x
    print *, "x - 0 =", result

    result = x/1.0  ! Should become: result = x
    print *, "x / 1 =", result

    ! Zero operations - should be simplified
    result = x*0.0  ! Should become: result = 0.0
    print *, "x * 0 =", result

    result = 0.0 + x  ! Should become: result = x
    print *, "0 + x =", result

    ! Strength reduction - expensive operations to cheaper ones
    result = x*2.0  ! May become: x + x (addition cheaper than multiplication)
    print *, "x * 2 =", result

    result = x**2  ! May become: x * x (multiplication cheaper than power)
    print *, "x^2 =", result

    int_result = int_x*4  ! May become: int_x << 2 (bit shift)
    print *, "int_x * 4 =", int_result

    ! Associativity and commutativity optimizations
    result = (x + y) + 5.0  ! May be reordered for better constant folding
    print *, "(x + y) + 5 =", result

    result = x*(y*2.0)  ! May become: x * y * 2.0 or (x * y) * 2.0
    print *, "x * (y * 2) =", result

    ! Distribution and factoring
    result = x*y + x*2.0  ! May become: x * (y + 2.0)
    print *, "x*y + x*2 =", result

    ! More complex algebraic patterns
    call demonstrate_advanced_simplifications()

    print *, ""
    print *, "Expected MLIR optimizations:"
    print *, "- Identity operations eliminated"
    print *, "- Zero operations simplified"
    print *, "- Strength reduction applied"
    print *, "- Expressions reordered for optimization"
    print *, "- Common subexpressions factored"

contains

    subroutine demonstrate_advanced_simplifications()
        real :: a = 4.0
        real :: b = 6.0
        real :: temp_result

        print *, ""
        print *, "Advanced algebraic simplifications:"

        ! Double negation
        temp_result = -(-a)  ! Should become: temp_result = a
        print *, "-(-a) =", temp_result

        ! Subtraction patterns
        temp_result = a - a  ! Should become: temp_result = 0.0
        print *, "a - a =", temp_result

        temp_result = a - 0.0  ! Should become: temp_result = a
        print *, "a - 0 =", temp_result

        ! Division patterns
        temp_result = a/a  ! Should become: temp_result = 1.0 (if a != 0)
        print *, "a / a =", temp_result

        temp_result = 0.0/a  ! Should become: temp_result = 0.0 (if a != 0)
        print *, "0 / a =", temp_result

        ! Complex expressions with simplification opportunities
        temp_result = (a + b) - a  ! Should become: temp_result = b
        print *, "(a + b) - a =", temp_result

        temp_result = (a*b)/a  ! Should become: temp_result = b (if a != 0)
        print *, "(a * b) / a =", temp_result

        ! Distributive law
        temp_result = a*(b + 1.0)  ! May stay as is or become: a*b + a
        print *, "a * (b + 1) =", temp_result

        ! Boolean algebra (with logical operations)
        call demonstrate_logical_simplifications()

        ! Comparison simplifications
        call demonstrate_comparison_simplifications()
    end subroutine demonstrate_advanced_simplifications

    subroutine demonstrate_logical_simplifications()
        logical :: p = .true.
        logical :: q = .false.
        logical :: log_result

        print *, ""
        print *, "Logical simplifications:"

        ! Identity laws
        log_result = p .and. .true.   ! Should become: log_result = p
        print *, "p AND true =", log_result

        log_result = p .or. .false.   ! Should become: log_result = p
        print *, "p OR false =", log_result

        ! Domination laws
        log_result = p .and. .false.  ! Should become: log_result = .false.
        print *, "p AND false =", log_result

        log_result = p .or. .true.    ! Should become: log_result = .true.
        print *, "p OR true =", log_result

        ! Idempotence
        log_result = p .and. p        ! Should become: log_result = p
        print *, "p AND p =", log_result

        log_result = p .or. p         ! Should become: log_result = p
        print *, "p OR p =", log_result

        ! Double negation
        log_result = .not. (.not. p)  ! Should become: log_result = p
        print *, "NOT (NOT p) =", log_result

        ! De Morgan's laws (may be applied in reverse for optimization)
        log_result = .not. (p .and. q)  ! Equivalent to: (.not. p) .or. (.not. q)
        print *, "NOT (p AND q) =", log_result
    end subroutine demonstrate_logical_simplifications

    subroutine demonstrate_comparison_simplifications()
        real :: val1 = 10.0
        real :: val2 = 10.0
        logical :: comp_result

        print *, ""
        print *, "Comparison simplifications:"

        ! Constant comparisons
        comp_result = 5.0 > 3.0       ! Should become: comp_result = .true.
        print *, "5 > 3 =", comp_result

        comp_result = 2.0 == 2.0      ! Should become: comp_result = .true.
        print *, "2 == 2 =", comp_result

        ! Self-comparisons
        comp_result = val1 == val1    ! Should become: comp_result = .true.
        print *, "val1 == val1 =", comp_result

        comp_result = val1 > val1     ! Should become: comp_result = .false.
        print *, "val1 > val1 =", comp_result

        ! Redundant comparisons
        if (val1 >= 0.0 .and. val1 > -1.0) then  ! Second condition is redundant
            print *, "Redundant comparison case"
        end if
    end subroutine demonstrate_comparison_simplifications

end program algebraic_simplification
