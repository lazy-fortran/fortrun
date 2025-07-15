# Current Test Failures

## 1. control_flow_simple.f - PARTIAL SUCCESS (Exit code 1)

**File**: `example/notebook/control_flow_simple.f`

**Status Update**: 
- ✅ **DO LOOP FIXED** - do loop variables now properly declared
- ✅ **SEGFAULT FIXED** - do while loops no longer cause segmentation faults
- ✅ **DO LOOP PARSING** - do loops correctly parsed as do_loop_node
- ✅ **VARIABLE DECLARATION** - loop variable 'i' now declared as integer
- ⚠️ **PARTIAL FAILURE** - other constructs may still have issues

**Fixed Issues**:
- Do loop variable declaration: `integer :: i` is now generated
- Do loop execution: properly prints "Count: 1", "Count: 2", etc.
- Parse do loops as proper AST nodes instead of "unknown"

**Remaining Issues**:
- Test still fails (exit code 1) but do loops work correctly
- May be related to do while loops or select case statements
- Need further investigation of other language constructs

**Successful Test Case**:
```fortran
do i = 1, 5
    print *, i
end do
```
Now generates:
```fortran
program main
    implicit none
    integer :: i
    do i = 1, 5
        print *, i
    end do
end program main
```

**Next Steps**:
- Investigate remaining compilation issues in control_flow_simple.f
- Check do while loops and select case statements
- Test other complex language constructs