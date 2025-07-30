program loop_optimization
    ! Demonstrates loop optimization techniques
    ! Including loop unrolling, invariant hoisting, and vectorization opportunities

    integer, parameter :: n = 1000
    real :: array1(n), array2(n), array3(n)
    real :: scalar = 2.5
    integer :: i
    real :: sum = 0.0
    real :: invariant_result

    print *, "Loop Optimization Example"
    print *, ""

    ! Initialize arrays
    call initialize_arrays()

    ! Example 1: Loop with invariant computation
    ! The scalar*scalar computation should be hoisted out of the loop
    print *, "Loop with invariant computation:"
    do i = 1, 100
        array1(i) = array2(i) + scalar*scalar  ! scalar*scalar is loop invariant
    end do
    print *, "Completed invariant hoisting example"

    ! Example 2: Simple vectorizable loop
    ! This should be recognized as vectorizable
    print *, "Vectorizable loop:"
    do i = 1, n
        array3(i) = array1(i) + array2(i)  ! Simple element-wise addition
    end do
    print *, "Completed vectorization example"

    ! Example 3: Loop unrolling candidate
    ! Small loop with simple body - good candidate for unrolling
    print *, "Loop unrolling candidate:"
    do i = 1, 8
        sum = sum + array1(i)*2.0  ! Simple operation, small iteration count
    end do
    print *, "Sum from unrolled loop:", sum

    ! Example 4: Loop with stride optimization
    call stride_optimization_example()

    ! Example 5: Nested loop optimization
    call nested_loop_example()

    print *, ""
    print *, "Expected MLIR optimizations:"
    print *, "- Loop invariant computations hoisted"
    print *, "- Small loops unrolled"
    print *, "- Vectorizable loops identified"
    print *, "- Memory access patterns optimized"

contains

    subroutine initialize_arrays()
        integer :: i

        do i = 1, n
            array1(i) = real(i)
            array2(i) = real(i)*0.5
            array3(i) = 0.0
        end do
    end subroutine initialize_arrays

    subroutine stride_optimization_example()
        integer :: i
        real :: temp_sum = 0.0

        print *, "Stride optimization example:"

        ! Access with stride 2 - may benefit from optimization
        do i = 1, n, 2
            temp_sum = temp_sum + array1(i)
        end do
        print *, "Stride sum:", temp_sum

        ! Compare with unit stride access
        temp_sum = 0.0
        do i = 1, n/2
            temp_sum = temp_sum + array1(i)
        end do
        print *, "Unit stride sum:", temp_sum
    end subroutine stride_optimization_example

    subroutine nested_loop_example()
        integer, parameter :: m = 50
        real :: matrix(m, m)
        real :: vector(m)
        real :: result(m)
        integer :: i, j

        print *, "Nested loop optimization:"

        ! Initialize test data
        do i = 1, m
            vector(i) = real(i)
            do j = 1, m
                matrix(i, j) = real(i + j)
            end do
        end do

        ! Matrix-vector multiplication
        ! This should be optimized for cache efficiency and possibly vectorized
        do i = 1, m
            result(i) = 0.0
            do j = 1, m
                result(i) = result(i) + matrix(i, j)*vector(j)
            end do
        end do

        print *, "Matrix-vector multiplication completed"
        print *, "First result element:", result(1)
        print *, "Last result element:", result(m)

        ! Alternative loop ordering (may have different optimization characteristics)
        call demonstrate_loop_interchange(matrix, vector, result, m)
    end subroutine nested_loop_example

    subroutine demonstrate_loop_interchange(matrix, vector, result, m)
        integer, intent(in) :: m
        real, intent(in) :: matrix(m, m)
        real, intent(in) :: vector(m)
        real, intent(out) :: result(m)
        integer :: i, j

        ! Different loop order - may affect optimization
        result = 0.0  ! Initialize

        do j = 1, m
            do i = 1, m
                result(i) = result(i) + matrix(i, j)*vector(j)
            end do
        end do

        print *, "Alternative loop order completed"
    end subroutine demonstrate_loop_interchange

end program loop_optimization
