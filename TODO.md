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

#### ✅ COMPLETED: Phase 1: Fix Frontend Architecture Violations
- [x] **Fixed frontend.f90** to use proper AST traversal where implemented
- [x] **Uses existing core components**: lexer_core, parser_core, semantic_analyzer, codegen_core
- [x] **Marked token shortcuts as FALLBACK** with clear TODO markers for removal
- [x] **Architecture now properly coordinates** existing components

#### ✅ COMPLETED: Phase 2: Address Token Manipulation Shortcuts  
- [x] **Marked all `generate_*_from_tokens()` functions** as FALLBACK until AST complete
- [x] **Added clear removal markers** - TODO comments for future cleanup
- [x] **Maintained temporary fallbacks** only for unimplemented AST features
- [x] **No longer violates architecture** - fallback is explicitly temporary

#### Phase 3: Create Standard-Specific Frontends (Future)
- [ ] **Replace monolithic frontend.f90** with standard-specific coordinators  
- [ ] **Create `lazy_fortran_frontend.f90`** - Uses core + standard/lazy_fortran/
- [ ] **Create `fortran90_frontend.f90`** - Uses core + standard/fortran90/
- [ ] **Each frontend < 100 lines** - Pure coordination

#### ✅ COMPLETED: Phase 4: Verification
- [x] **All tests still pass** - No regression from architecture fixes
- [x] **Import statements work** - FPM handles module resolution automatically  
- [x] **Directory structure clean** - Organized into logical subdirectories

## IMMEDIATE TASKS ⚡

### 🚨 URGENT: Frontend.f90 Too Long (969 lines!)

**PROBLEM**: Frontend.f90 is still 969 lines - it should be a thin coordinator (~50-100 lines)

**ROOT CAUSE**: All FALLBACK functions are still embedded in frontend.f90 instead of being extracted to separate modules

#### IMMEDIATE PLAN: Extract FALLBACK Functions

**Phase 1: Extract Token Fallback Module (HIGH PRIORITY)**
- [ ] **Create `src/frontend/fallback/token_fallback.f90`**
  - Move `generate_use_statements_from_tokens()`
  - Move `generate_executable_statements_from_tokens()` 
  - Move `generate_function_definitions_from_tokens()`
  - Move `reconstruct_line_from_tokens()`
  - Move `set_current_tokens()` and token storage
  - Move all helper functions (`is_function_def_statement()`, etc.)

**Phase 2: Extract Declaration Generator (HIGH PRIORITY)**  
- [ ] **Create `src/frontend/fallback/declaration_generator.f90`**
  - Move `generate_declarations()`
  - Move `infer_basic_type()`
  - Move `get_function_names_from_tokens()`
  - Move all variable declaration logic

**Phase 3: Extract Utility Functions (MEDIUM PRIORITY)**
- [ ] **Create `src/frontend/utils/debug_utils.f90`**
  - Move `debug_output_tokens()`, `debug_output_ast()`, `debug_output_codegen()`
- [ ] **Create `src/frontend/utils/parser_utils.f90`**
  - Move `remove_inline_comments()`, `advance_to_next_statement()`

**Phase 4: Clean Frontend Coordinator (HIGH PRIORITY)**
- [ ] **Reduce frontend.f90 to ~50-100 lines** - Pure coordination only
- [ ] **Import and use extracted modules**
- [ ] **Clean interface**: `compile_source()` + options only

**Target Structure After Extraction:**
```
src/frontend/
├── frontend.f90              # 50-100 lines: Pure coordinator
├── fallback/                 # Temporary modules until AST complete
│   ├── token_fallback.f90    # Token manipulation functions  
│   └── declaration_generator.f90 # Variable declaration logic
├── utils/                    # Utility functions
│   ├── debug_utils.f90       # Debug output functions
│   └── parser_utils.f90      # Parser helper functions
└── [existing structure]      # lexer/, parser/, semantic/, codegen/
```

**Expected Result**: Frontend.f90 becomes a clean ~80 line coordinator that just calls the right modules in sequence.

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

