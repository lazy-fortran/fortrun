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
  !! Note: This module is now largely obsolete as type inference handles most cases
  !! Kept for backward compatibility and potential future pattern-based extensions
  subroutine detect_missing_variables(input_file, unit_out, scope_var_count)
    character(len=*), intent(in) :: input_file
    integer, intent(in) :: unit_out
    integer, intent(in) :: scope_var_count
    
    ! Type inference now handles variable detection automatically
    ! This module remains available for future extensibility but is no longer needed
    ! for the core .f file examples that previously required hardcoded patterns
    
    ! Only apply patterns if no variables were already detected
    if (scope_var_count > 0) return
    
    ! Only apply to .f files  
    if (index(input_file, '.f') == 0 .or. index(input_file, '.f90') > 0) return
    
    ! All previous hardcoded patterns removed - type inference handles these cases:
    ! - calculator.f: assignment detection finds x, y, sum, product
    ! - real_default_test.f: sizeof() detection finds x, x4, x8
    ! - simple_math.f: assignment detection finds variables
    ! - Function calls and arithmetic expressions properly analyzed
    
    ! Future: Could add sophisticated pattern matching here if needed for edge cases
    
  end subroutine detect_missing_variables

  ! Previous hardcoded pattern functions removed - no longer needed!
  ! Type inference automatically handles:
  ! - calculator.f: assignment detection finds x, y, sum, product
  ! - real_default_test.f: sizeof() detection finds x, x4, x8  
  ! - simple_math.f: assignment detection finds variables
  ! - arrays.f: type inference handles array declarations
  ! - derived_types.f: type inference handles variable detection
  ! - function_returns.f: function analysis handles return types
  ! - notebook files: loop variable detection works automatically
  !
  ! Future pattern-based enhancements can be added here if needed for edge cases

end module variable_pattern_detector