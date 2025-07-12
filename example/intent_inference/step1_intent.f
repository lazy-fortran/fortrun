! Step 1: Intent inference with explicit types
call test_sub(5.0, result)
print *, "Result:", result

subroutine test_sub(input, output)
    real, intent(in) :: input    ! Explicit intent(in) 
    real, intent(out) :: output  ! Explicit intent(out)
    output = input * 2.0
end subroutine