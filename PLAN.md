# EMERGENCY REPAIR PLAN - FIX ALL FAILURES

## IMMEDIATE EXECUTION PLAN - NO DELAYS

### PHASE 1: STOP THE BLEEDING (SEGFAULTS)
**PRIORITY: CRITICAL - SYSTEM UNUSABLE**

1. **Disable broken type system HM temporarily**
   - Comment out all Hindley-Milner type inference calls
   - Replace with simple type assignments (real(8), integer)
   - Remove segfaulting substitution code
   - Get basic functionality working first

2. **Fix CLI argument parsing crashes**
   - Add defensive null checks in cli.f90:49
   - Fix memory allocation issues
   - Ensure all CLI tests pass

### PHASE 2: FIX CODE GENERATION (INVALID FORTRAN)
**PRIORITY: CRITICAL - BROKEN OUTPUT**

1. **Fix print statement generation**
   ```fortran
   ! BROKEN: "print * , "Hello""
   ! FIXED:  "print *, "Hello""
   ```
   - Remove extra space before comma in codegen_core.f90
   - Fix all print statement formatting

2. **Fix program structure generation**
   - Ensure proper program/end program blocks
   - Fix variable declaration ordering
   - Generate valid Fortran syntax

### PHASE 3: CLEAN BUILD SYSTEM
**PRIORITY: HIGH - BUILD CONFLICTS**

1. **Remove duplicate modules**
   - Delete test_module_out.f90 duplicates
   - Clean up conflicting test files
   - Ensure unique module names

2. **Fix missing notebook dependencies**
   - Create missing notebook_output.f90
   - Create missing figure_capture.f90
   - Or disable notebook tests temporarily

### PHASE 4: SYSTEMATIC TEST FIXING
**PRIORITY: HIGH - RESTORE FUNCTIONALITY**

1. **Frontend tests (2 failing)**
   - Fix test_frontend_statements
   - Fix test_example_test_cases
   - All must pass with correct formatting

2. **CLI tests (3 failing)**
   - Fix test_cli_cache
   - Fix test_cli_debug 
   - Fix test_cli_system
   - No more segfaults allowed

3. **Runner tests (2 failing)**
   - Fix test_runner_edge_cases
   - Fix test_runner_comprehensive
   - Handle all edge cases properly

4. **Semantic tests (3 failing)**
   - Fix type inference without crashes
   - Handle basic type unification
   - No segfaults in type system

5. **Example tests (2 failing)**
   - Fix all example programs
   - Ensure consistent output
   - All examples must work

6. **Notebook tests (2 failing)**
   - Create missing dependencies
   - Fix integration tests
   - Or disable if not critical

### PHASE 5: RESTORE TYPE SYSTEM SAFELY
**PRIORITY: MEDIUM - ADVANCED FEATURES**

1. **Rebuild type system incrementally**
   - Start with simple types only
   - Add defensive programming everywhere
   - Test each addition thoroughly
   - No crashes allowed

2. **Add proper error handling**
   - Graceful degradation on type errors
   - Clear error messages
   - No segfaults ever

## EXECUTION ORDER

1. **IMMEDIATE (TODAY)**
   - Phase 1: Stop segfaults
   - Phase 2: Fix code generation
   - Get basic functionality working

2. **NEXT (WITHIN 24H)**
   - Phase 3: Clean build system
   - Phase 4: Fix all tests systematically
   - 100% test pass rate required

3. **LATER (WHEN STABLE)**
   - Phase 5: Restore advanced type system
   - Add new features only after stability

## SUCCESS CRITERIA

- **0 segmentation faults** - System must be stable
- **0 test failures** - All 37 tests must pass
- **Valid Fortran output** - All generated code must compile
- **No build conflicts** - Clean compilation every time

## WORKING METHODOLOGY

1. **One issue at a time** - No parallel work
2. **Test after each fix** - Verify immediately
3. **Commit working fixes** - Incremental progress
4. **No compromises** - Fix everything properly

## NON-NEGOTIABLES

- **ALL TESTS MUST PASS**
- **NO SEGFAULTS ALLOWED**
- **VALID CODE GENERATION ONLY**
- **STABLE BUILD SYSTEM**

**START PHASE 1 IMMEDIATELY. NO DELAYS.**