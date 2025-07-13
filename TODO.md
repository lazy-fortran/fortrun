# AST Implementation TODO

This document tracks the implementation plan for the AST-based architecture.

## Completed Phases ✅

- **Phase 0**: Test Reorganization
- **Phase 1**: Lexer Implementation  
- **Phase 2**: AST Definition
- **Phase 3**: Parser Implementation (partial)
- **Phase 4**: Code Generation (partial)
- **Phase 5**: AST-Based Preprocessor Integration (basic)
- **Phase 6**: Cache Management Enhancement

## Current: Phase 7 - Advanced Features 🚧

### Parser Tasks
- [ ] Handle implicit program wrapping for Simple Fortran
- [ ] Support function/subroutine definitions
- [ ] Error recovery and detailed error reporting
- [ ] JSON serialization of parse trees

### Parser Test Cases
- [ ] `test_parser_statements.f90` - Statement parsing (assignments, prints)
- [ ] `test_parser_functions.f90` - Function/subroutine parsing
- [ ] `test_parser_programs.f90` - Full program parsing with implicit wrapping
- [ ] `test_parser_errors.f90` - Error recovery and reporting
- [ ] `test_parser_serialization.f90` - AST to JSON serialization

### Code Generation Tasks
- [ ] Apply modern defaults (real(8), etc.)
- [ ] Handle indentation and formatting
- [ ] Generate contains statements

### Code Generation Test Cases
- [ ] `test_codegen_functions.f90` - Function generation
- [ ] `test_codegen_defaults.f90` - Modern defaults application
- [ ] `test_codegen_formatting.f90` - Code formatting

### AST Preprocessor Tasks
- [ ] Support functions and subroutines
- [ ] Support arrays and derived types
- [ ] Advanced type inference
- [ ] Preserve comments and markdown cells
- [ ] Symbol table and scope management

### AST Preprocessor Test Cases
- [ ] `test_preprocessor_ast_functions.f90` - Function preprocessing
- [ ] `test_preprocessor_ast_arrays.f90` - Array preprocessing

### Known Limitations
- Arrays not yet supported
- Derived types not yet supported
- Functions/subroutines not yet supported
- Comments and markdown cells not preserved

## Phase 8: Full Integration 📋

- [ ] Ensure all examples work with AST preprocessor
- [ ] Remove legacy preprocessor code
- [ ] Update all documentation
- [ ] Performance optimization
- [ ] Remove FORTRAN_USE_AST_PREPROCESSOR environment variable

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