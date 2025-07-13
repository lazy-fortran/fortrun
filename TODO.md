# AST Implementation TODO

This document tracks the implementation plan for the AST-based architecture.

## IMMEDIATE TASKS ⚡

### ❗ CRITICAL: Test Cleanup and Deduplication
- [ ] **URGENT**: Read through ALL test files in each category before deciding to delete
- [ ] **URGENT**: Understand what each test does and why it exists
- [ ] **URGENT**: Only delete tests that are genuinely flakey, shallow, or nonsensical after proper analysis
- [ ] **URGENT**: Remove duplication in tests - keep only the deepest test that covers a specific portion of code
- [ ] **URGENT**: For each code area, identify the most comprehensive test and remove redundant ones
- [ ] **URGENT**: Rearrange and rename test source files as needed while keeping category naming convention
- [ ] **URGENT**: Create new focused tests if gaps are found after removing duplicates
- [ ] **URGENT**: Go through each test category systematically: cache/, config/, figure/, lexer/, parser/, etc.
- [ ] **URGENT**: Document decision for each test file (keep/delete/rename and why)

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

## Current: Phase 7 - Proper AST-Based Code Generation 🚧

**CRITICAL**: The current AST preprocessor falls back to line-by-line reconstruction instead of proper AST-based code generation. This defeats the purpose of having an AST and needs immediate fixing.

### Immediate Priority: Implement Selective AST Fallback
- [ ] ❗ **URGENT**: Replace `process_line_simple()` with proper AST parsing for supported features
- [ ] ❗ **URGENT**: Use `parse_statement()` and AST nodes for assignments, USE statements, print statements
- [ ] ❗ **URGENT**: Implement proper AST-based code generation via `generate_fortran()` for supported features
- [ ] ❗ **URGENT**: Use line reconstruction ONLY as selective fallback for unsupported features (temporarily)
- [ ] ❗ **URGENT**: Track and minimize fallback usage over time

### Parser Tasks (Proper AST Implementation)
- [ ] Parse USE statements into AST nodes
- [ ] Parse assignment statements into AST nodes
- [ ] Parse function/subroutine calls into AST nodes
- [ ] Parse print statements into AST nodes
- [ ] Handle implicit program wrapping for Simple Fortran
- [ ] Support function/subroutine definitions
- [ ] Error recovery and detailed error reporting
- [ ] JSON serialization of parse trees

### Code Generation Tasks (Proper AST Implementation)
- [ ] Generate USE statements from AST nodes
- [ ] Generate assignment statements from AST nodes
- [ ] Generate function calls from AST nodes  
- [ ] Generate print statements from AST nodes
- [ ] Apply modern defaults (real(8), etc.) during generation
- [ ] Handle proper indentation and formatting
- [ ] Generate contains statements
- [ ] Ensure correct statement ordering (USE → implicit none → declarations → code)

### Test Cases (Write Tests FIRST - TDD!)
- [ ] `test_ast_use_statements.f90` - USE statement parsing and generation
- [ ] `test_ast_assignments.f90` - Assignment parsing and generation
- [ ] `test_ast_function_calls.f90` - Function call parsing and generation
- [ ] `test_ast_print_statements.f90` - Print statement parsing and generation
- [ ] `test_parser_statements.f90` - Statement parsing (assignments, prints)
- [ ] `test_parser_functions.f90` - Function/subroutine parsing
- [ ] `test_parser_programs.f90` - Full program parsing with implicit wrapping
- [ ] `test_codegen_functions.f90` - Function generation
- [ ] `test_codegen_defaults.f90` - Modern defaults application
- [ ] `test_codegen_formatting.f90` - Code formatting

### Architecture Fix Required
The current `preprocessor_ast.f90` needs rewrite to use selective fallbacks:
1. Parse source into proper AST using existing parser modules for supported features
2. Transform AST (type inference, implicit program wrapping)
3. Generate Fortran code from AST using existing codegen modules for supported features
4. Use line reconstruction ONLY as selective fallback for temporarily unsupported features
5. **OLD legacy preprocessor deleted** - all functionality moved to AST preprocessor
6. **Minimize fallback usage** - track which features still need fallback and implement them properly

### Known Issues to Fix
- ❌ Line-by-line reconstruction bypasses AST benefits
- ❌ USE statements handled as raw text instead of AST nodes
- ❌ No proper statement ordering
- ❌ No type inference integration
- ❌ No symbol table usage

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