# *Lazy Fortran* Compiler Frontend TODO

This document tracks concrete implementation tasks for the *lazy fortran* compiler frontend.
Our architectural plans and designs live in `doc/plan/` directory, while this TODO.md is for specific actionable tasks.

## Vision
We are building a complete compiler frontend with a 4-phase architecture (Lexer → Parser → Semantic Analysis → Code Generation) that can target multiple backends. Our *lazy fortran* dialect pushes beyond all alternative scientific computing languages, exploring how far we can evolve Fortran to surpass Python, Julia, MATLAB, and others in both performance and expressiveness. Currently, we use standard Fortran as our intermediate representation, which allows immediate use with existing Fortran compilers.

## IMMEDIATE TASKS ⚡

### Fix Existing Test Suite 🚨
After recent enhancements (multi-statement parsing, enhanced type inference), we need to ensure all existing tests still pass:

- [ ] **Fix compilation errors** in new test files (character length mismatches)
- [ ] **Run full test suite** and fix any regressions
- [ ] **Verify core functionality**:
  - [ ] Basic lazy fortran compilation (x = 42)
  - [ ] Type inference for all types (integer, real, character)
  - [ ] Multi-statement parsing
  - [ ] Expression type promotion
  - [ ] Standard Fortran passthrough (.f90 files)
- [ ] **Update integration tests** to match new multi-statement capabilities
- [ ] **Ensure CI/CD passes** before continuing with new features

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
- [x] **COMPLETED**: Read through ALL test files in each category before deciding to delete
- [x] **COMPLETED**: Understand what each test does and why it exists
- [x] **COMPLETED**: Only delete tests that are genuinely flakey, shallow, or nonsensical after proper analysis
- [x] **COMPLETED**: Remove duplication in tests - keep only the deepest test that covers a specific portion of code
- [x] **COMPLETED**: For each code area, identify the most comprehensive test and remove redundant ones
- [x] **COMPLETED**: Go through each test category systematically: cache/, config/, cli/, preprocessor/, lexer/, type/
- [x] **COMPLETED**: Removed 19 redundant test files (94 → 75 files) while maintaining complete coverage

#### Test Hierarchy Strategy
- **Unit tests**: Test individual functions/modules in isolation
- **Integration tests**: Test interaction between components
- **System tests**: Test full end-to-end functionality
- **Keep only ONE comprehensive test per category that covers the deepest level needed**
- **Remove shallow "smoke tests" that just print PASS without real verification**

## Completed Phases ✅

- **Phase 0**: Test Reorganization
- **Phase 1**: Lexer Implementation  
- **Phase 2**: AST Definition
- **Phase 3**: Parser Implementation (partial)
- **Phase 4**: Code Generation (partial)
- **Phase 5**: AST-Based Preprocessor Integration (basic)
- **Phase 6**: Cache Management Enhancement

## ✅ Completed: Phase 7 - Proper AST-Based Code Generation

**SUCCESS**: Implemented selective AST fallback architecture with proper AST parsing for core features and selective fallback for complex cases.

### ✅ Completed: Selective AST Fallback Implementation
- [x] ✅ **COMPLETED**: Replaced line reconstruction with proper AST parsing for supported features
- [x] ✅ **COMPLETED**: Use `parse_statement()` and AST nodes for assignments, USE statements, print statements
- [x] ✅ **COMPLETED**: Implemented proper AST-based code generation via `generate_code()` for supported features
- [x] ✅ **COMPLETED**: Use line reconstruction ONLY as selective fallback for unsupported features (temporarily)
- [x] ✅ **COMPLETED**: Made AST preprocessor the default implementation (`preprocess_file()`)

### ✅ Architecture Successfully Implemented
- [x] ✅ **NEW**: `preprocess_file_ast_based()` function with proper AST parsing
- [x] ✅ **NEW**: Selective fallback mechanism for unsupported features
- [x] ✅ **NEW**: Proper statement ordering (USE → implicit none → declarations → code)
- [x] ✅ **NEW**: Automatic type inference and variable declarations
- [x] ✅ **NEW**: Assignment statements via `parse_statement()` and `generate_code()`

### ✅ Completed Parser Tasks
- [x] ✅ Parse assignment statements into AST nodes via `parse_statement()`
- [x] ✅ Parse USE statements with proper collection and ordering
- [x] ✅ Parse print statements with AST detection and selective fallback  
- [x] ✅ Handle implicit program wrapping for Simple Fortran
- [x] ✅ Basic type inference for literals (integer, real, string)

### ✅ Completed Code Generation Tasks
- [x] ✅ Generate assignment statements from AST nodes via `generate_code()`
- [x] ✅ Generate USE statements with proper ordering (before implicit none)
- [x] ✅ Generate print statements via selective fallback
- [x] ✅ Apply modern defaults (real(8), integer) during type inference
- [x] ✅ Handle proper indentation and formatting
- [x] ✅ Ensure correct statement ordering (USE → implicit none → declarations → code)

### ✅ Completed Test Cases (TDD Implementation)
- [x] ✅ `test_ast_assignments.f90` - Assignment parsing and generation ✅
- [x] ✅ `test_ast_use_statements.f90` - USE statement parsing and generation ✅
- [x] ✅ `test_ast_print_statements.f90` - Print statement parsing and generation ✅
- [x] ✅ `test_ast_based_processing.f90` - Integration testing ✅
- [x] ✅ `test_ast_complex_expressions.f90` - Selective fallback testing ✅

### ✅ Architecture Successfully Implemented
1. ✅ Parse source into proper AST using existing parser modules for supported features
2. ✅ Transform AST (type inference, implicit program wrapping)  
3. ✅ Generate Fortran code from AST using existing codegen modules for supported features
4. ✅ Use line reconstruction ONLY as selective fallback for temporarily unsupported features
5. ✅ **AST preprocessor is now the default** - `preprocess_file()` calls AST-based implementation
6. ✅ **Selective fallback working** - complex expressions use fallback, core features use AST

### ✅ Major Issues Resolved
- ✅ Line-by-line reconstruction replaced with proper AST parsing for core features
- ✅ USE statements handled with proper AST collection and ordering
- ✅ Proper statement ordering implemented (USE → implicit none → declarations → code)
- ✅ Type inference integration working for basic types
- ✅ Assignment statements use full AST pipeline (`parse_statement()` → `generate_code()`)

### Remaining Tasks for Future Phases
- [ ] Parse function/subroutine calls into AST nodes (Phase 8+)
- [ ] Support function/subroutine definitions (Phase 8+)
- [ ] Enhanced comment handling for production examples
- [ ] Advanced string type inference with proper length detection
- [ ] Error recovery and detailed error reporting
- [ ] JSON serialization of parse trees

## ✅ COMPLETED: Phase 8 - JSON Debug Serialization

## ✅ COMPLETED: Phase 9 - Architecture Fixed with Hindley-Milner Type System

## ✅ COMPLETED: Phase 10 - Frontend Architecture Reorganization

## ✅ COMPLETED: Phase 11 - *lazy fortran* Compiler Frontend Working! 🚀

## ✅ COMPLETED: Phase 12 - Frontend Runtime Issues Fixed! 🎉

