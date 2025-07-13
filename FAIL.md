# Test Failures Report

This document tracks current test failures to guide development priorities.

## Test Run Summary

Date: 2025-07-13
Branch: ast
Test Command: `fpm test`

## Critical Issues

### 1. USE Statement Placement Errors
**Error**: "USE statement at (1) cannot follow data declaration statement at (2)"
**Affected**: Multiple example programs
**Issue**: Frontend is placing USE statements after variable declarations instead of before
**Priority**: HIGH - This breaks Fortran syntax rules

### 2. Function Declaration Issues
**Error**: "Procedure 'X' called with an implicit interface" and "Function 'X' at (1) has no IMPLICIT type"
**Affected**: Examples with function calls (add, multiply, calculate_area, etc.)
**Issue**: Frontend not generating proper function declarations or interfaces
**Priority**: HIGH - Missing core functionality

### 3. Function Definition Parsing
**Error**: "Syntax error in data declaration at (1)" and "'square' at (1) is not a variable"
**Affected**: Examples with function definitions
**Issue**: Parser doesn't handle function definitions properly
**Priority**: HIGH - Core language feature missing

### 4. Program Structure Errors
**Error**: "Syntax error in END PROGRAM statement" and "Expecting END PROGRAM statement"
**Affected**: Various test programs
**Issue**: Malformed program structure generation
**Priority**: MEDIUM - Affects program completeness

### 5. Notebook Module Dependencies
**Error**: "Unable to find source for module dependency: notebook_output"
**Affected**: Notebook tests
**Issue**: Missing module dependency
**Priority**: LOW - Feature-specific issue

## Passing Tests

- Basic error handling tests
- Cache management tests
- Example test cases (use_statement, print_statement, multi_statement)
- Frontend statement tests

## Root Cause Analysis

The main issues stem from:
1. **Statement ordering** - USE statements placed after declarations
2. **Missing parser support** - Functions, subroutines not properly parsed
3. **Incomplete code generation** - Missing function interfaces and declarations

## Next Steps

1. Fix USE statement placement in frontend code generation
2. Add function/subroutine parsing support to parser
3. Implement function declaration generation
4. Add proper program structure validation
5. Fix notebook module dependencies

## Test Categories Status

- ✅ Basic frontend functionality (assignments, use, print)
- ✅ Error handling
- ✅ Cache management
- ❌ Function definitions and calls
- ❌ Complex program structures
- ❌ Example programs with functions
- ❌ Notebook functionality