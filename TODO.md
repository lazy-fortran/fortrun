# AST Implementation TODO

This document tracks the implementation plan for replacing the current line-based preprocessor with a modern lexer/parser/AST architecture as described in `doc/plan/AST.md`.

## Goal

Replace the existing preprocessor with a clean AST-based architecture while maintaining support for all current examples and test cases.

## Phase 0: Test Reorganization ✅

Reorganize test directory to enable targeted testing with FPM wildcards.

### Tasks
- [x] Create new test directory structure: `test/<subsystem>/`
- [x] Move and rename tests following pattern: `test_<subsystem>_<case>.f90`
- [x] Update `fpm.toml` to support new structure (auto-discovery works)
- [x] Verify all tests still run with `fpm test`
- [x] Document wildcard usage (e.g., `fpm test test_lexer_*`)

### Usage Examples
```bash
# Run all tests
fpm test

# Run all cache tests
fpm test test_cache*

# Run all preprocessor tests
fpm test test_preprocessor*

# Run all type inference tests
fpm test test_type*

# Run specific test
fpm test test_cache_lock
```

### New Test Structure
```
test/
├── cli/
│   ├── test_cli_basic.f90
│   ├── test_cli_system.f90
│   └── test_cli_verbose.f90
├── preprocessor/
│   ├── test_preprocessor_basic.f90
│   ├── test_preprocessor_integration.f90
│   ├── test_preprocessor_function.f90
│   └── test_preprocessor_debug.f90
├── type_inference/
│   ├── test_type_inference_basic.f90
│   ├── test_type_inference_step1.f90
│   └── test_type_inference_literals.f90
├── cache/
│   └── test_cache_basic.f90
├── module_scanner/
│   └── test_module_scanner_basic.f90
├── registry/
│   └── test_registry_resolver.f90
├── fpm_generator/
│   └── test_fpm_generator_basic.f90
├── examples/
│   └── test_examples_all.f90
└── lexer/  (new)
    ├── test_lexer_basic.f90
    ├── test_lexer_tokens.f90
    └── test_lexer_errors.f90
```

## Phase 1: Lexer Implementation

Build a tokenizer for the Simple Fortran subset.

### Tasks
- [ ] Create `src/lexer.f90` module
- [ ] Define token types (identifier, number, string, operator, keyword, etc.)
- [ ] Implement tokenization for basic constructs
- [ ] Write unit tests for lexer
- [ ] Support error reporting with line/column information
- [ ] Handle comments and whitespace
- [ ] Benchmark lexer performance

### Test Cases
- [ ] `test_lexer_basic.f90` - Token generation for simple statements
- [ ] `test_lexer_numbers.f90` - Integer and real literal tokenization
- [ ] `test_lexer_operators.f90` - Operator tokenization
- [ ] `test_lexer_keywords.f90` - Keyword recognition
- [ ] `test_lexer_errors.f90` - Error handling and recovery

## Phase 2: AST Definition

Define AST node types for Simple Fortran.

### Tasks
- [ ] Create `src/ast.f90` module with base node type
- [ ] Implement node types:
  - [ ] Program node (implicit/explicit)
  - [ ] Assignment node
  - [ ] Binary operation node
  - [ ] Function/subroutine definition
  - [ ] Function call node
  - [ ] Variable reference
  - [ ] Literal values
  - [ ] Use statement
  - [ ] Print statement
- [ ] Implement visitor pattern for AST traversal
- [ ] Write unit tests for AST construction

### Test Cases
- [ ] `test_ast_construction.f90` - Manual AST building
- [ ] `test_ast_visitor.f90` - Visitor pattern tests

## Phase 3: Parser Implementation

Build recursive descent parser for Simple Fortran.

### Tasks
- [ ] Create `src/parser.f90` module
- [ ] Implement expression parsing (precedence climbing)
- [ ] Implement statement parsing
- [ ] Handle implicit program wrapping
- [ ] Support function/subroutine definitions
- [ ] Error recovery and reporting
- [ ] Write comprehensive parser tests

### Test Cases
- [ ] `test_parser_expressions.f90` - Expression parsing
- [ ] `test_parser_statements.f90` - Statement parsing
- [ ] `test_parser_functions.f90` - Function/subroutine parsing
- [ ] `test_parser_programs.f90` - Full program parsing
- [ ] `test_parser_errors.f90` - Error recovery

## Phase 4: Semantic Analysis

Integrate type inference and symbol table management.

### Tasks
- [ ] Create `src/semantic_analyzer.f90` module
- [ ] Integrate existing type inference modules
- [ ] Build symbol table with scoping
- [ ] Implement type checking
- [ ] Handle implicit none injection
- [ ] Add intent(in) defaults
- [ ] Write semantic analysis tests

### Test Cases
- [ ] `test_semantic_types.f90` - Type inference integration
- [ ] `test_semantic_symbols.f90` - Symbol table tests
- [ ] `test_semantic_scopes.f90` - Scope management
- [ ] `test_semantic_defaults.f90` - Modern defaults application

## Phase 5: Code Generation

Generate Fortran code from AST.

### Tasks
- [ ] Create `src/codegen_fortran.f90` module
- [ ] Implement AST to Fortran transformation
- [ ] Apply modern defaults (real(8), etc.)
- [ ] Handle indentation and formatting
- [ ] Generate contains statements
- [ ] Write code generation tests

### Test Cases
- [ ] `test_codegen_basic.f90` - Basic code generation
- [ ] `test_codegen_functions.f90` - Function generation
- [ ] `test_codegen_defaults.f90` - Modern defaults application
- [ ] `test_codegen_formatting.f90` - Code formatting

## Phase 6: Integration

Replace preprocessor with new AST-based system.

### Tasks
- [ ] Create `src/compiler_frontend.f90` to coordinate phases
- [ ] Update `src/preprocessor.f90` to use new architecture
- [ ] Ensure all existing examples work
- [ ] Run all existing tests
- [ ] Fix any compatibility issues
- [ ] Performance benchmarking vs old preprocessor

### Test Cases
- [ ] All existing preprocessor tests should pass
- [ ] All examples should work unchanged
- [ ] Performance should be equal or better

## Phase 7: Cleanup

Remove old code and finalize implementation.

### Tasks
- [ ] Remove old line-based processing code
- [ ] Clean up module interfaces
- [ ] Update documentation
- [ ] Add architecture documentation
- [ ] Final performance optimization
- [ ] Update CLAUDE.md with new architecture

## Success Criteria

1. All existing tests pass with new implementation
2. All examples work without modification
3. Performance is equal or better than current preprocessor
4. Code is more maintainable and extensible
5. Architecture supports future features (multiple dispatch, IR generation)

## Testing Strategy

- Write tests first (TDD approach)
- Each phase has dedicated unit tests
- Integration tests verify phase interactions
- Existing tests serve as regression suite
- Use `fpm test test_<subsystem>_*` for targeted testing

## Notes

- Reuse existing modules where possible (type inference, cache, etc.)
- Maintain backward compatibility throughout
- Focus on clean interfaces between phases
- Keep performance in mind from the start
- Document design decisions as we go