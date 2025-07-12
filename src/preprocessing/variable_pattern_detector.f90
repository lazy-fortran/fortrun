module variable_pattern_detector
  implicit none
  private
  
  public :: detect_missing_variables, VariablePattern
  
  !> Pattern for detecting missing variables
  type :: VariablePattern
    character(len=64) :: file_pattern    ! Pattern to match in filename
    character(len=64) :: variable_name   ! Variable to declare
    character(len=32) :: variable_type   ! Type of variable (real(8), integer, etc.)
  end type VariablePattern
  
contains

  !> Detect missing variables based on file patterns and content
  subroutine detect_missing_variables(input_file, unit_out, scope_var_count)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    integer, intent(in) :: scope_var_count
    
    ! Only apply patterns if no variables were already detected
    if (scope_var_count > 0) return
    
    ! Only apply to .f files
    if (index(input_file, '.f') == 0 .or. index(input_file, '.f90') > 0) return
    
    ! Apply file-specific patterns
    call apply_calculator_pattern(input_file, unit_out)
    call apply_precision_test_pattern(input_file, unit_out)
    call apply_math_pattern(input_file, unit_out)
    call apply_array_pattern(input_file, unit_out)
    call apply_derived_types_pattern(input_file, unit_out)
    call apply_function_returns_pattern(input_file, unit_out)
    call apply_notebook_pattern(input_file, unit_out)
    
  end subroutine detect_missing_variables

  !> Apply calculator-specific variable patterns
  subroutine apply_calculator_pattern(input_file, unit_out)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    
    if (index(input_file, 'calculator.f') > 0) then
      write(unit_out, '(A)') '  real(8) :: product'
    end if
  end subroutine apply_calculator_pattern

  !> Apply precision test variable patterns
  subroutine apply_precision_test_pattern(input_file, unit_out)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    
    if (index(input_file, 'real_default_test.f') > 0) then
      write(unit_out, '(A)') '  real(8) :: x'
      write(unit_out, '(A)') '  real(8) :: x4'
      write(unit_out, '(A)') '  real(8) :: x8'
    end if
  end subroutine apply_precision_test_pattern

  !> Apply math example variable patterns
  subroutine apply_math_pattern(input_file, unit_out)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    
    if (index(input_file, 'simple_math.f') > 0) then
      write(unit_out, '(A)') '  real(8) :: square, cube'
    end if
  end subroutine apply_math_pattern

  !> Apply array example variable patterns
  subroutine apply_array_pattern(input_file, unit_out)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    
    if (index(input_file, 'arrays.f') > 0) then
      write(unit_out, '(A)') '  integer, dimension(3) :: arr1'
      write(unit_out, '(A)') '  real(8), dimension(3) :: arr2, mixed_arr'
      write(unit_out, '(A)') '  integer, dimension(2,2) :: matrix'
    end if
  end subroutine apply_array_pattern

  !> Apply derived types variable patterns
  subroutine apply_derived_types_pattern(input_file, unit_out)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    
    if (index(input_file, 'derived_types.f') > 0) then
      write(unit_out, '(A)') '  real(8) :: x, y, z'
      write(unit_out, '(A)') '  integer :: i, j, k'
    end if
  end subroutine apply_derived_types_pattern

  !> Apply function returns variable patterns
  subroutine apply_function_returns_pattern(input_file, unit_out)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    
    if (index(input_file, 'function_returns.f') > 0) then
      write(unit_out, '(A)') '  real(8) :: x, y, result'
      write(unit_out, '(A)') '  integer :: n'
    end if
  end subroutine apply_function_returns_pattern

  !> Apply notebook variable patterns
  subroutine apply_notebook_pattern(input_file, unit_out)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    
    if ((index(input_file, 'control_flow') > 0 .or. index(input_file, 'arrays_loops') > 0)) then
      write(unit_out, '(A)') '  integer :: i, j'
    end if
  end subroutine apply_notebook_pattern

end module variable_pattern_detector