program test_step1_debug
    use preprocessor, only: preprocess_file
    implicit none
    
    character(len=256) :: input_file, output_file, error_msg
    character(len=1024) :: line
    integer :: unit, ios
    
    ! Test the multiple functions case
    input_file = 'test_multi_func.f'
    output_file = 'test_multi_func.f90'
    
    ! Create test file
    open(newunit=unit, file=input_file, status='replace')
    write(unit, '(A)') 'x = 5.0'
    write(unit, '(A)') 'y = square(x)'
    write(unit, '(A)') 'z = cube(x)'
    write(unit, '(A)') 'print *, x, y, z'
    write(unit, '(A)') ''
    write(unit, '(A)') 'real function square(val)'
    write(unit, '(A)') '  real :: val'
    write(unit, '(A)') '  square = val * val'
    write(unit, '(A)') 'end function'
    write(unit, '(A)') ''
    write(unit, '(A)') 'real function cube(val)'
    write(unit, '(A)') '  real :: val'
    write(unit, '(A)') '  cube = val * val * val'
    write(unit, '(A)') 'end function'
    close(unit)
    
    ! Process the file
    call preprocess_file(input_file, output_file, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, 'Error:', trim(error_msg)
        stop 1
    end if
    
    ! Check output
    print *, '=== Generated output ==='
    open(newunit=unit, file=output_file, status='old')
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        print *, trim(line)
    end do
    close(unit)
    
    ! Check for required patterns
    print *, ''
    print *, '=== Checking for required patterns ==='
    print *, 'Looking for: real(8) :: x'
    call check_pattern(output_file, 'real(8) :: x')
    print *, 'Looking for: real(8) :: y'
    call check_pattern(output_file, 'real(8) :: y')
    print *, 'Looking for: real(8) :: z'
    call check_pattern(output_file, 'real(8) :: z')
    print *, 'Looking for: real(8) function square'
    call check_pattern(output_file, 'real(8) function square')
    print *, 'Looking for: real(8) function cube'
    call check_pattern(output_file, 'real(8) function cube')
    
contains

    subroutine check_pattern(filename, pattern)
        character(len=*), intent(in) :: filename, pattern
        character(len=1024) :: line
        logical :: found
        integer :: unit, ios
        
        found = .false.
        open(newunit=unit, file=filename, status='old')
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, trim(pattern)) > 0) then
                found = .true.
                exit
            end if
        end do
        close(unit)
        
        if (found) then
            print *, '  FOUND: ', trim(pattern)
        else
            print *, '  NOT FOUND: ', trim(pattern)
        end if
    end subroutine

end program test_step1_debug