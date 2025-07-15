# Current Test Failures

## 1. control_flow_simple.f - COMPILATION ERROR (Exit code 1)

**File**: `example/notebook/control_flow_simple.f`

**Error**: 
```
Error: Symbol 'i' at (1) has no IMPLICIT type
```

**Root Cause**: 
- The file contains `do i = 1, 5` loops where the loop variable 'i' is not declared
- In lazy fortran dialect, type inference should handle loop variables automatically  
- The type inference system doesn't recognize loop variables from do loops

**Status**: 
- ✅ SEGFAULT FIXED - do while loops no longer cause segmentation faults
- ✅ DO WHILE PARSING IMPLEMENTED - basic do while loop parsing is working
- ⚠️ TYPE INFERENCE ISSUE - loop variables not being inferred/declared

**Fix Required**:
- Ensure the do loop is properly parsed as a do_loop_node (not "unknown")
- The declaration generator already handles do_loop_node variables
- Need to debug why do loops are not being parsed correctly

**Test Case**:
```fortran
do i = 1, 5
    print *, i
end do
```
Should generate:
```fortran
program main
    implicit none
    integer :: i
    do i = 1, 5
        print *, i
    end do
end program main
```