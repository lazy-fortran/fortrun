# *Lazy Fortran* Compiler Frontend TODO

This document tracks concrete implementation tasks for the *lazy fortran* compiler frontend.
Our architectural plans and designs live in `doc/plan/` directory, while this TODO.md is for specific actionable tasks.

## Vision
We are building a complete compiler frontend with a 4-phase architecture (Lexer → Parser → Semantic Analysis → Code Generation) that can target multiple backends. Our *lazy fortran* dialect pushes beyond all alternative scientific computing languages, exploring how far we can evolve Fortran to surpass Python, Julia, MATLAB, and others in both performance and expressiveness. Currently, we use standard Fortran as our intermediate representation, which allows immediate use with existing Fortran compilers.

## IMMEDIATE TASKS ⚡

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

## ✅ Completed: Phase 8 - JSON Debug Serialization

**SUCCESS**: Implemented JSON debug serialization for all compiler stages (tokens, AST, codegen).

### ✅ Completed: Debug Infrastructure
- [x] ✅ **COMPLETED**: Added --debug-tokens flag for token JSON output
- [x] ✅ **COMPLETED**: Added --debug-ast flag for AST JSON output  
- [x] ✅ **COMPLETED**: Added --debug-codegen flag for codegen JSON output
- [x] ✅ **COMPLETED**: Implemented global debug_state module for flag management
- [x] ✅ **COMPLETED**: Fixed lexer comment handling bug (comments no longer tokenized)
- [x] ✅ **COMPLETED**: Fixed preprocessor bounds checking for EOF-only lines

### ✅ Completed: JSON Serialization
- [x] ✅ **COMPLETED**: Token JSON serialization with proper type names
- [x] ✅ **COMPLETED**: AST JSON serialization with proper field names (target, value, left, right)
- [x] ✅ **COMPLETED**: Codegen JSON serialization showing input → output transformation
- [x] ✅ **COMPLETED**: All JSON outputs are well-formatted and debuggable

### ✅ Completed: Test Cases
- [x] ✅ Token debug: `fortran example.f --debug-tokens` → example_tokens.json
- [x] ✅ AST debug: `fortran example.f --debug-ast` → example_ast.json
- [x] ✅ Codegen debug: `fortran example.f --debug-codegen` → example_codegen.json
- [x] ✅ Combined: All three flags can be used together

## ✅ COMPLETED: Phase 9 - Architecture Fixed with Hindley-Milner Type System! 🎉

### ✅ Successfully Implemented Clean 4-Phase Pipeline:
1. **Lexer** → Tokens ✅
2. **Parser** → AST (NO type inference!) ✅
3. **Semantic Analysis** → Type inference with Hindley-Milner ✅
4. **Code Generation** → Generate code using inferred types ✅

### ✅ Completed Architecture Components:

#### ✅ Type System Foundation
- [x] **Created `src/core/type_system.f90`** with full Hindley-Milner types:
  - [x] `type_var_t` - Type variables with automatic naming ('a, 'b, etc.)
  - [x] `mono_type_t` - Monomorphic types (int, real, char(n), array, function)
  - [x] `poly_type_t` - Type schemes with quantified variables
  - [x] `type_env_t` - Type environment with lookup/extend/generalize
  - [x] `substitution_t` - Type substitutions with composition

#### ✅ Semantic Analyzer Implementation  
- [x] **Created `src/core/semantic_analyzer.f90`** with Algorithm W:
  - [x] `infer()` - Complete type inference for all AST nodes
  - [x] `unify()` - Sound unification with occurs check
  - [x] `instantiate()` - Correct instantiation of type schemes
  - [x] `generalize()` - Proper generalization with free variable analysis
  - [x] `fresh_type_var()` - Unique type variable generation

#### ✅ AST Integration
- [x] **Updated `src/core/ast_core.f90`**:
  - [x] Added `type(mono_type_t), allocatable :: inferred_type` to base ast_node
  - [x] All node types now carry type information after semantic analysis
  
#### ✅ Clean Preprocessor
- [x] **Cleaned up `src/parser/preprocessor.f90`**:
  - [x] Removed `track_variable_type()` subroutine completely
  - [x] Removed all var_names, var_types, var_count tracking
  - [x] Parser now focuses purely on syntax, not types

#### ✅ New 4-Phase Pipeline
- [x] **Created `src/parser/preprocessor_new.f90`** with proper pipeline:
  - [x] Phase 1: Complete tokenization of entire file
  - [x] Phase 2: Parse all tokens into complete AST
  - [x] Phase 3: Run semantic analysis on entire AST
  - [x] Phase 4: Generate code with type-directed declarations

### Next Steps:
- [ ] Replace old preprocessor with new 4-phase implementation
- [ ] Add comprehensive tests for type inference
- [ ] Handle more complex language features

## ✅ COMPLETED: Phase 10 - Frontend Architecture Reorganization

### ✅ Successfully Reorganized as Compiler Frontend:
1. **Created dedicated frontend directory structure** ✅:
   ```
   src/frontend/
   ├── semantic/
   │   ├── type_system_hm.f90    # Hindley-Milner types
   │   └── semantic_analyzer.f90  # Type inference  
   ├── ast_typed.f90              # AST with type info
   ├── frontend.f90               # Main frontend interface
   └── frontend_integration.f90   # Integration layer
   ```

2. **Renamed modules to avoid conflicts** ✅:
   - Created `type_system_hm` for Hindley-Milner types
   - Created `semantic_analyzer` in frontend/
   - Kept existing `type_system` for backward compatibility

3. **Created unified frontend interface** ✅:
   - Single entry point for all compilation phases
   - Backend selection (currently Fortran as IR)
   - Clean API integrated with existing tools

4. **Updated all references** ✅:
   - Changed "preprocessor" → "frontend" throughout codebase
   - Updated documentation (README.md, CLAUDE.md, doc/index.md)
   - Integrated with main runner and notebook executor

### Phase 9: Advanced AST Features 📋

### Core AST Working Examples ✅
- [x] ✅ Simple assignments work perfectly (`x = 42`, `y = 3.14`)
- [x] ✅ Basic programs work (`hello.f` example)
- [x] ✅ Type inference and print statements work
- [x] ✅ Clean examples without comments work flawlessly
- [x] ✅ Enhanced comment handling for production examples with inline comments
- [x] ✅ Improved string type inference for character variables
- [x] ✅ Function call parsing in expressions

### Integration Tasks
- [x] ✅ **COMPLETED**: AST preprocessor is now the default (legacy available as `preprocess_file_legacy()`)

### Documentation and Polish
- [x] ✅ **COMPLETED**: Updated TODO.md to reflect Phase 7 completion
- [x] ✅ **COMPLETED**: Updated TODO.md to reflect Phase 8 completion
- [ ] Update README and documentation to reflect AST-based architecture
- [ ] Create examples showcasing AST preprocessor capabilities

## ✅ Completed: Serialization Tasks

- [x] ✅ **COMPLETED**: Added `to_json()` method to token type using json-fortran
- [x] ✅ **COMPLETED**: Added `to_json()` visitor for AST nodes using json-fortran
- [x] ✅ **COMPLETED**: Created JSON writer wrapper module for consistent formatting
- [x] ✅ **COMPLETED**: Added debug flags for JSON output of each stage

## Success Criteria

1. All existing tests pass with new implementation
2. All examples work without modification
3. Code is more maintainable and extensible
4. Architecture supports future features
5. All intermediate stages are inspectable via JSON serialization
6. **Implementation follows strict TDD (red-green-refactor) cycle**
7. **Clean 4-phase separation with Hindley-Milner type inference**

## Important Notes

⚠️ **ALWAYS clear the cache before testing new features!**

```bash
# Clear cache
fortran --clear-cache

# Then run your test
fortran example.f90
```

- Maintain backward compatibility throughout
- Focus on clean interfaces between phases
- Document design decisions as we go
- Use JSON for all serialization