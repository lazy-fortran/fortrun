# Test-Driven Development Backlog

This document outlines the remaining features to implement in the Fortran compiler frontend, organized by priority with RED tests that should be written first.

## High Priority - Core Language Features

### 1. Array Constructors with Implied DO Loops ✓
**Status**: COMPLETED - Parser, semantic analysis, and codegen all working

**RED Test**:
```fortran
! test/frontend/test_implied_do_constructor.f90
squares = [(i*i, i=1,5)]        ! Should generate [1, 4, 9, 16, 25]
evens = [(2*i, i=1,10)]         ! Should generate [2, 4, 6, ..., 20]
matrix = [((i+j, i=1,3), j=1,3)] ! Nested implied do
```

**Implementation Tasks**:
- [x] Fix semantic analyzer to handle loop variables in implied DO
- [x] Implement type inference for implied DO expressions
- [x] Generate proper code for array construction

### 2. STOP and RETURN Statements ✓
**Status**: COMPLETED - Tokens, parsing, AST nodes, and code generation all working

**RED Test**:
```fortran
! test/frontend/test_stop_return_statements.f90
program test_stop
    if (x < 0) stop "Error: negative value"
    stop 0
end program

function compute(x)
    if (x == 0) return
    compute = x * 2
    return
end function
```

**Implementation Tasks**:
- [x] Add STOP token and parsing
- [x] Add RETURN token and parsing
- [x] Implement AST nodes for both statements
- [x] Generate appropriate code

### 3. Basic Intrinsic Functions ✓
**Status**: COMPLETED - Basic intrinsic functions with array support working

**RED Test**:
```fortran
! test/frontend/test_intrinsic_functions.f90
arr = [1, 2, 3, 4, 5]
n = size(arr)           ! Should be 5
s = sum(arr)            ! Should be 15
dims = shape(arr)       ! Should be [5]
m = max(1, 2, 3)        ! Should be 3
root = sqrt(16.0)       ! Should be 4.0
```

**Implementation Tasks**:
- [x] Create intrinsic function registry
- [x] Implement type signatures for each intrinsic
- [x] Add semantic analysis for intrinsic calls
- [x] Generate appropriate code or runtime calls

## Medium Priority - Control Flow

### 4. CYCLE and EXIT Statements ✓
**Status**: COMPLETED - Tokens, parsing, AST nodes, and code generation all working

**RED Test**:
```fortran
! test/frontend/test_cycle_exit.f90
do i = 1, 10
    if (i == 3) cycle      ! Skip iteration
    if (i == 7) exit       ! Break loop
    print *, i
end do

outer: do i = 1, 3
    inner: do j = 1, 3
        if (i == j) cycle outer
        print *, i, j
    end do inner
end do outer
```

**Implementation Tasks**:
- [x] Add CYCLE and EXIT tokens
- [x] Parse with optional loop labels
- [x] Implement control flow in AST
- [x] Generate proper jump statements

### 5. WHERE Construct ✓
**Status**: COMPLETED - Both single-line and multi-line WHERE constructs working

**RED Test**:
```fortran
! test/frontend/test_where_construct.f90
real :: a(10), b(10)
where (a > 0)
    b = sqrt(a)
elsewhere
    b = 0.0
end where

where (a > 0) b = 1.0/a    ! Single line form
```

**Implementation Tasks**:
- [x] Add WHERE/ELSEWHERE/END WHERE parsing
- [x] Create WHERE AST node with mask expression
- [x] Implement array masking in code generation
- [x] Handle nested WHERE constructs

### 6. Character String Operations ✓
**Status**: COMPLETED - Deferred length strings and string intrinsics working

**RED Test**:
```fortran
! test/frontend/test_string_operations.f90
character(len=10) :: str1
character(len=*), parameter :: str2 = "Hello"
character(len=:), allocatable :: str3

str1 = "World"
str3 = str1 // str2      ! Concatenation with allocation
n = len(str1)            ! Length intrinsic
n = len_trim(str1)       ! Trimmed length
```

**Implementation Tasks**:
- [x] Implement deferred length strings (len=:)
- [x] Add string intrinsics (LEN, LEN_TRIM, TRIM, etc.)
- [x] Proper type inference for string operations
- [x] Handle automatic allocation/reallocation

## Medium Priority - Type System

### 7. INTENT Attribute for Parameters ✓
**Status**: COMPLETED - INTENT parsing and storage implemented, violations caught by gfortran

**RED Test**:
```fortran
! test/frontend/test_intent_attribute.f90
subroutine process(input, output, workspace)
    real, intent(in) :: input(:)
    real, intent(out) :: output(:)
    real, intent(inout) :: workspace(:)
    
    output = input * 2.0   ! OK
    input = 0.0            ! Should error: can't modify intent(in)
end subroutine
```

**Implementation Tasks**:
- [x] Parse INTENT attribute in declarations
- [x] Add intent tracking to parameter AST nodes
- [x] Violations caught by gfortran backend
- [ ] (Optional) Implement semantic checks for earlier error detection

### 8. OPTIONAL Parameters ✓
**Status**: COMPLETED - OPTIONAL attribute and PRESENT intrinsic working

**RED Test**:
```fortran
! test/frontend/test_optional_params.f90
subroutine compute(x, y, tol)
    real :: x, y
    real, optional :: tol
    real :: tolerance
    
    if (present(tol)) then
        tolerance = tol
    else
        tolerance = 1.0e-6
    end if
end subroutine

call compute(1.0, 2.0)        ! Without optional
call compute(1.0, 2.0, 0.01)  ! With optional
```

**Implementation Tasks**:
- [x] Parse OPTIONAL attribute
- [x] Implement PRESENT intrinsic
- [x] Handle optional arguments in calls
- [x] Type check with optional parameters

## Low Priority - Advanced Features

### 9. Basic I/O Statements
**Status**: PRINT exists, others missing

**RED Test**:
```fortran
! test/frontend/test_io_statements.f90
integer :: unit, iostat
character(len=100) :: line

open(unit=10, file="data.txt", status="old", iostat=iostat)
if (iostat /= 0) stop "Cannot open file"

read(10, '(A)') line
write(*, '(A,I5)') "Count:", n

close(10)
```

**Implementation Tasks**:
- [ ] Implement OPEN statement parsing
- [ ] Implement READ/WRITE statement parsing  
- [ ] Add CLOSE statement
- [ ] Basic FORMAT string support
- [ ] Handle I/O error conditions

### 10. PARAMETER (Named Constants)
**Status**: Not implemented

**RED Test**:
```fortran
! test/frontend/test_parameter_constants.f90
real, parameter :: PI = 3.14159265359
integer, parameter :: MAX_SIZE = 1000
character(len=*), parameter :: VERSION = "1.0.0"
real, parameter :: ARRAY(*) = [1.0, 2.0, 3.0]

radius = PI * r * r     ! Using constant
```

**Implementation Tasks**:
- [ ] Parse PARAMETER attribute
- [ ] Evaluate constant expressions at compile time
- [ ] Store in symbol table as constants
- [ ] Forbid assignment to parameters
- [ ] Handle array constants

## Testing Infrastructure

### 11. Comprehensive Test Suite
**Status**: Tests exist but need organization

**Tasks**:
- [ ] Create test naming convention guide
- [ ] Organize tests by feature area
- [ ] Add integration tests for feature combinations
- [ ] Create performance benchmarks
- [ ] Add error message quality tests

### 12. Pipeline Tests
**Status**: Some failing tests need fixing

**Tasks**:
- [ ] Fix remaining parser error tests
- [ ] Complete semantic analysis test coverage
- [ ] Add standardizer edge case tests
- [ ] Test error recovery scenarios
- [ ] Add multi-file compilation tests

## Type Inference Enhancements

### 13. Advanced Type Inference
**Status**: Basic inference works, complex cases missing

**RED Test**:
```fortran
! test/frontend/test_advanced_type_inference.f90
! Should infer: arr is integer array of size 5
arr = [1, 2, 3, 4, 5]

! Should infer: result is real
result = sum(arr) / size(arr)

! Should infer: matrix is 2D integer array
matrix = reshape([1,2,3,4], [2,2])

! Should infer function return types
get_value = compute(x) + process(y)
```

**Implementation Tasks**:
- [ ] Infer array sizes from literals
- [ ] Propagate types through intrinsic functions
- [ ] Handle reshape and array manipulation
- [ ] Improve function return type inference
- [ ] Add type inference for string lengths

## Success Criteria

Each feature is considered complete when:
1. All RED tests pass
2. Edge cases are handled with appropriate error messages  
3. Generated code compiles with gfortran
4. Documentation is updated
5. No regression in existing tests

## Development Process

1. Write RED test first
2. Implement minimal code to make test pass
3. Refactor for clarity and performance
4. Add edge case tests
5. Update documentation
6. Run full test suite

## Priority Guidelines

- **High**: Core features needed for basic Fortran programs
- **Medium**: Features that improve usability significantly  
- **Low**: Advanced features used in specialized contexts

Focus on completing all high priority items before moving to medium priority.