! Test that existing intent is preserved
call compute(result)

subroutine compute(output)
  real, intent(out) :: output
  output = 42.0
end subroutine