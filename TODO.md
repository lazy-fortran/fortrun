# TODO: Critical Fix for gfortran Allocatable Bug

## CRITICAL ISSUE DISCOVERED
gfortran has a severe bug where functions returning allocatable types cause memory corruption when the result is not allocated. This manifests as segfaults, double-free errors, and random crashes.

## CURRENT STATUS (2025-07-18) - RESOLVED
- Converted all allocatable-returning functions to subroutines in type_system_hm.f90 and scope_manager.f90
- Fixed double-free errors by implementing assignment operator overloading for deep copies
- **SOLUTION**: Added `assignment(=)` operators to `mono_type_t` and `poly_type_t` that perform deep copies
- Semantic analysis is now re-enabled and working correctly
- All tests pass without double-free errors

## FINAL SOLUTION: Assignment Operator Overloading

The root cause of double-free errors was shallow copies when storing types in the AST. The solution was to overload the assignment operator for type system types:

```fortran
type :: mono_type_t
    ! ... fields ...
contains
    procedure :: assign => mono_type_assign
    generic :: assignment(=) => assign
end type

type :: poly_type_t
    ! ... fields ...
contains
    procedure :: assign => poly_type_assign
    generic :: assignment(=) => assign
end type
```

Now all assignments like `node%inferred_type = inferred` automatically perform deep copies, preventing shared ownership of allocatable components.

## COMPLETED ACTIONS

### Phase 1: Convert Critical Allocatable-Returning Functions (URGENT)
These functions directly return allocatable types and MUST be converted to subroutines:

1. **type_system_hm.f90**:
   - [x] `subst_lookup()` - returns `type(mono_type_t), allocatable` ✓ CONVERTED
   - [x] `env_lookup()` - returns `type(poly_type_t), allocatable` ✓ CONVERTED
   - [x] `free_type_vars()` - returns `type(type_var_t), allocatable :: vars(:)` ✓ CONVERTED

2. **scope_manager.f90**:
   - [x] `scope_lookup()` - returns `type(poly_type_t), allocatable` ✓ CONVERTED
   - [x] `stack_lookup()` - returns `type(poly_type_t), allocatable` ✓ CONVERTED

3. **semantic_analyzer.f90**:
   - [x] `get_env_free_vars()` - returns `type(type_var_t), allocatable :: vars(:)` ✓ CONVERTED
   - [x] `get_scheme_free_vars()` - returns `type(type_var_t), allocatable :: vars(:)` ✓ CONVERTED

### Phase 2: Review Functions Returning Types with Allocatable Components
These functions return types that contain allocatable components. Need to ensure all components are properly initialized:

1. **Constructor Functions** (may need to ensure all allocatable components are initialized):
   - [ ] `create_type_var()` - ensure `name` is allocated
   - [ ] `create_mono_type()` - ensure `args` handling is safe
   - [ ] `create_poly_type()` - ensure `forall` handling is safe
   - [ ] `create_scope()` - ensure all components initialized
   - [ ] `create_scope_stack()` - ensure array allocated

2. **Deep Copy Functions** (critical - must handle unallocated components):
   - [ ] `mono_type_deep_copy()` - handle unallocated `args`
   - [ ] `poly_type_deep_copy()` - handle unallocated `forall`

3. **Transformation Functions**:
   - [ ] All `infer_*` functions returning `mono_type_t`
   - [ ] `apply_substitution()` and related functions
   - [ ] `env_remove()` and `env_apply_subst()` returning `type_env_t`

### Phase 3: Pattern Conversion

Convert from:
```fortran
function lookup(name) result(scheme)
    type(poly_type_t), allocatable :: scheme
    ! ... may return unallocated
end function

! Usage:
result = lookup("x")  ! CRASHES if unallocated!
```

To:
```fortran
subroutine lookup(name, scheme)
    character(len=*), intent(in) :: name
    type(poly_type_t), allocatable, intent(out) :: scheme
    ! ... safe to leave unallocated
end subroutine

! Usage:
call lookup("x", result)  ! Safe!
```

### Phase 4: Update All Callers
After converting functions to subroutines, update all call sites throughout:
- [ ] semantic_analyzer.f90
- [ ] type_checker.f90
- [ ] All test files
- [ ] Any other modules using these functions

### Phase 5: Testing Strategy
1. [ ] Create comprehensive test for each converted function
2. [ ] Test both allocated and unallocated return cases
3. [ ] Run with `-fcheck=all` to catch any remaining issues
4. [ ] Use valgrind for memory leak detection

## Long-term Improvements

1. **Coding Standards**:
   - Never use functions returning allocatable types in gfortran
   - Always use subroutines with `intent(out)` allocatable parameters
   - Document this limitation prominently

2. **Alternative Patterns**:
   - Consider using optional arguments for "not found" cases
   - Use error codes or status flags instead of unallocated returns
   - Implement proper error handling throughout

3. **Compiler Workarounds**:
   - Test with different gfortran versions
   - Consider reporting bug to GCC if not already known
   - Document minimum compiler versions once fixed

## Notes
- This bug affects gfortran 12.2.0 (confirmed)
- May affect other versions - needs testing
- Intel Fortran and other compilers may handle this differently
