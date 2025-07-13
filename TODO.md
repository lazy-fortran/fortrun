# *Lazy Fortran* Compiler Frontend TODO

This document tracks concrete implementation tasks for the *lazy fortran* compiler frontend.
Our architectural plans and designs live in `doc/plan/` directory, while this TODO.md is for specific actionable tasks.

## Vision
We are building a complete compiler frontend with a 4-phase architecture (Lexer → Parser → Semantic Analysis → Code Generation) that can target multiple backends. Our *lazy fortran* dialect pushes beyond all alternative scientific computing languages, exploring how far we can evolve Fortran to surpass Python, Julia, MATLAB, and others in both performance and expressiveness. Currently, we use standard Fortran as our intermediate representation, which allows immediate use with existing Fortran compilers.

## CRITICAL ARCHITECTURE VIOLATION ⚠️

**URGENT**: The current frontend.f90 implementation violates our core architecture by taking shortcuts directly from tokens to code generation, bypassing the AST pipeline.

### What's Wrong:
- Direct token manipulation in `generate_*_from_tokens()` functions
- String reconstruction from tokens instead of AST traversal
- Bypassing semantic analysis for code generation
- Violates clean separation of Lexer → Parser → AST → Semantic Analysis → Code Generation

### Required Fixes (CORRECTED):

#### ✅ COMPLETED: Directory Reorganization
- [x] **Move semantic analysis** → `src/core/` (semantic_analyzer.f90, etc.)
- [x] **Rename dialects/** → `src/standards/` (lazy_fortran, fortran90, fortran2018)
- [x] **Update documentation** to reflect actual structure

#### Phase 1: Fix Frontend Architecture Violations
- [ ] **Replace frontend.f90 reimplementations** with calls to existing core components:
  - Use `lexer_core.f90` instead of custom `lex_file()`
  - Use `parser_core.f90` instead of custom `parse_tokens()`  
  - Use `semantic_analyzer.f90` instead of custom type inference
  - Use `codegen_core.f90` instead of custom code generation

#### Phase 2: Remove Token Manipulation Shortcuts
- [ ] **Remove all `generate_*_from_tokens()` functions** - Architectural violations
- [ ] **Remove `reconstruct_line_from_tokens()` usage** - No string manipulation
- [ ] **Implement proper AST-based pipeline** - Use existing components

#### Phase 3: Create Standard-Specific Frontends  
- [ ] **Replace monolithic frontend.f90** with standard-specific coordinators
- [ ] **Create `lazy_fortran_frontend.f90`** - Uses core + standards/lazy_fortran/
- [ ] **Create `fortran90_frontend.f90`** - Uses core + standards/fortran90/
- [ ] **Each frontend < 100 lines** - Pure coordination

#### Phase 4: Verification
- [ ] **Ensure all tests still pass** - No regression
- [ ] **Update fpm.toml** for new structure  
- [ ] **Update all import statements** in source files

## IMMEDIATE TASKS ⚡

### ⚠️ URGENT: Fix Architecture Violations

### ✅ COMPLETED: Fix Existing Test Suite

### Standard Fortran Compatibility Tests 🔄
Since *lazy fortran* is a superset of standard Fortran, we need comprehensive tests to ensure any valid Fortran 95/2003/2008/2018 program passes through unchanged:

- [ ] **Create test/standard_fortran/** directory for compatibility tests
- [ ] **Fortran 95 Core Features**:
  - [ ] Program/module/subroutine/function structures
  - [ ] All intrinsic types and declarations
  - [ ] Arrays (static, dynamic, assumed-shape)
  - [ ] Control structures (if/then/else, do loops, select case)
  - [ ] Operators and expressions
  - [ ] Intrinsic functions
  - [ ] Format statements and I/O
  - [ ] Common blocks (legacy but required)
  - [ ] Data statements
  - [ ] Equivalence statements
  - [ ] Parameter statements
- [ ] **Fortran 2003 Features**:
  - [ ] Object-oriented programming constructs
  - [ ] Type-bound procedures
  - [ ] Abstract interfaces
  - [ ] Parameterized derived types
  - [ ] Allocatable components
- [ ] **Fortran 2008/2018 Features**:
  - [ ] Coarrays
  - [ ] Submodules
  - [ ] DO CONCURRENT
  - [ ] ERROR STOP
- [ ] **Test Infrastructure**:
  - [ ] Compare frontend output byte-for-byte with input for standard files
  - [ ] Test suite from Fortran standards committee examples
  - [ ] Real-world Fortran libraries (BLAS, LAPACK snippets)
  - [ ] Ensure no modifications to standard constructs

### ✅ COMPLETED: Test Cleanup and Deduplication

## Completed Phases ✅

- **Phase 0**: Test Reorganization
- **Phase 1**: Lexer Implementation  
- **Phase 2**: AST Definition
- **Phase 3**: Parser Implementation (partial)
- **Phase 4**: Code Generation (partial)
- **Phase 5**: AST-Based Preprocessor Integration (basic)
- **Phase 6**: Cache Management Enhancement

## ✅ Completed: Phase 7 - Proper AST-Based Code Generation

## ✅ COMPLETED: Phase 8 - JSON Debug Serialization

## ✅ COMPLETED: Phase 9 - Architecture Fixed with Hindley-Milner Type System

## ✅ COMPLETED: Phase 10 - Frontend Architecture Reorganization

## ✅ COMPLETED: Phase 11 - *lazy fortran* Compiler Frontend Working! 🚀

## ✅ COMPLETED: Phase 12 - Frontend Runtime Issues Fixed! 🎉

## ✅ COMPLETED: Phase 13 - Test Infrastructure Improvements

