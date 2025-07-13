# AST Implementation TODO

This document tracks the implementation plan for the AST-based architecture.

## IMMEDIATE TASKS ⚡

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

## Phase 8: Full Integration and Enhancement 📋

### Core AST Working Examples ✅
- [x] ✅ Simple assignments work perfectly (`x = 42`, `y = 3.14`)
- [x] ✅ Basic programs work (`hello.f` example)
- [x] ✅ Type inference and print statements work
- [x] ✅ Clean examples without comments work flawlessly

### Integration Tasks
- [ ] Enhanced comment handling for production examples with inline comments
- [ ] Improve string type inference for character variables
- [ ] Function call parsing in expressions (currently uses selective fallback)
- [ ] Performance optimization vs legacy preprocessor
- [x] ✅ **COMPLETED**: AST preprocessor is now the default (legacy available as `preprocess_file_legacy()`)

### Documentation and Polish
- [x] ✅ **COMPLETED**: Updated TODO.md to reflect Phase 7 completion
- [ ] Update README and documentation to reflect AST-based architecture
- [ ] Create examples showcasing AST preprocessor capabilities
- [ ] Performance benchmarking against legacy implementation

## Serialization Tasks

- [ ] Add `to_json()` method to token type using json-fortran
- [ ] Add `to_json()` visitor for AST nodes using json-fortran
- [ ] Create JSON writer wrapper module for consistent formatting
- [ ] Add serialization tests for each stage using json-fortran

## Success Criteria

1. All existing tests pass with new implementation
2. All examples work without modification
3. Performance is equal or better than current preprocessor
4. Code is more maintainable and extensible
5. Architecture supports future features
6. All intermediate stages are inspectable via JSON serialization
7. **Implementation follows strict TDD (red-green-refactor) cycle**

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