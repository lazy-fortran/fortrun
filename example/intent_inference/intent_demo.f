! Intent inference demonstration
! Shows how the preprocessor automatically infers intent(in), intent(out), intent(inout)

call demo_intents(5.0, result, counter)
print *, "Result:", result, "Counter:", counter

! Function with intent(in) - only reads parameter  
function square(x)
    square = x * x
end function

! Subroutine with intent(out) - only assigns to parameter
subroutine compute_result(output)
    output = 42.0
end subroutine

! Subroutine with intent(inout) - both reads and assigns parameter
subroutine increment_counter(count)
    count = count + 1
end subroutine

! Mixed intent demonstration
subroutine demo_intents(input, output, inout_param)
    output = square(input) * 2.0  ! output only assigned → intent(out)
    inout_param = inout_param + 1  ! inout_param read and assigned → intent(inout)
    ! input only read via square(input) → intent(in)
end subroutine