program test_feature_combinations
    implicit none
    
    ! Test combining multiple language features
    
    ! PARAMETER constants with array constructors
    integer, parameter :: SIZES(3) = [10, 20, 30]
    real, parameter :: PI = 3.14159265359
    
    ! Optional parameters with INTENT
    call test_optional_intent()
    
    ! WHERE construct with intrinsic functions
    call test_where_with_intrinsics()
    
    ! String operations with I/O
    call test_string_io()
    
    ! Array constructors with implied DO and type inference
    call test_complex_array_construction()
    
    print *, "All feature combination tests passed!"
    
contains

    subroutine test_optional_intent()
        real :: input(5), output(5)
        
        input = [1.0, 2.0, 3.0, 4.0, 5.0]
        call process_data(input, output)
        
        if (any(abs(output - input * 2.0) > 1e-6)) then
            error stop "Feature combo test failed: optional intent"
        end if
    end subroutine test_optional_intent
    
    subroutine process_data(x, y, scale)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        real, intent(in), optional :: scale
        real :: factor
        
        if (present(scale)) then
            factor = scale
        else
            factor = 2.0
        end if
        
        y = x * factor
    end subroutine process_data
    
    subroutine test_where_with_intrinsics()
        real :: data(10), result(10)
        integer :: i
        
        ! Create test data using implied DO
        data = [(sin(real(i) * PI / 10.0), i = 1, 10)]
        
        ! Use WHERE with intrinsic functions
        where (data > 0.0)
            result = sqrt(data)
        elsewhere
            result = -sqrt(abs(data))
        end where
        
        do i = 1, 10
            if (data(i) > 0.0) then
                if (abs(result(i) - sqrt(data(i))) > 1e-6) then
                    error stop "Feature combo test failed: where with intrinsics"
                end if
            else
                if (abs(result(i) + sqrt(abs(data(i)))) > 1e-6) then
                    error stop "Feature combo test failed: where with intrinsics"
                end if
            end if
        end do
    end subroutine test_where_with_intrinsics
    
    subroutine test_string_io()
        character(len=:), allocatable :: str1, str2, combined
        character(len=100) :: buffer
        integer :: unit, iostat
        
        ! String operations
        str1 = "Hello"
        str2 = "World"
        combined = trim(str1) // " " // trim(str2) // "!"
        
        ! Write to temporary file
        unit = 20
        open(unit=unit, status="scratch", iostat=iostat)
        if (iostat /= 0) then
            error stop "Feature combo test failed: cannot open scratch file"
        end if
        
        write(unit, '(A)') combined
        rewind(unit)
        read(unit, '(A)') buffer
        close(unit)
        
        if (trim(buffer) /= "Hello World!") then
            error stop "Feature combo test failed: string I/O"
        end if
    end subroutine test_string_io
    
    subroutine test_complex_array_construction()
        integer :: matrix(3,3), vector(9)
        integer :: i, j, k
        
        ! Complex array constructor with nested implied DO
        vector = [((i*j, j=1,3), i=1,3)]
        
        ! Reshape with type inference
        matrix = reshape(vector, [3, 3])
        
        ! Verify results
        k = 0
        do i = 1, 3
            do j = 1, 3
                k = k + 1
                if (vector(k) /= i*j) then
                    error stop "Feature combo test failed: array construction"
                end if
            end do
        end do
        
        ! Check matrix values
        do j = 1, 3
            do i = 1, 3
                if (matrix(i,j) /= vector((j-1)*3 + i)) then
                    error stop "Feature combo test failed: reshape"
                end if
            end do
        end do
    end subroutine test_complex_array_construction

end program test_feature_combinations