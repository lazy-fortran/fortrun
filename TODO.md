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
├── lexer/  (new)
│   ├── test_lexer_basic.f90
│   ├── test_lexer_tokens.f90
│   └── test_lexer_errors.f90
└── test_data/
    ├── lexer/
    │   ├── simple_assignment.f
    │   ├── simple_assignment.tokens.json  (expected output)
    │   └── ...
    ├── parser/
    │   ├── simple_assignment.ast.json
    │   └── ...
    └── codegen/
        ├── simple_assignment.f90
        └── ...
```

## Phase 1: Lexer Implementation - Core/Dialect Architecture

Build a tokenizer with shared core functionality and dialect-specific extensions.

### Tasks
- [x] Create directory structure: `src/core/`, `src/dialects/simple_fortran/`
- [x] Create `src/core/lexer_core.f90` module with standard Fortran tokenization
- [x] Define base token types shared by all dialects
- [ ] Create `src/dialects/simple_fortran/lexer_sf.f90` for Simple Fortran extensions
- [x] Create `src/lexer.f90` as main interface that delegates to appropriate implementation
- [x] Write unit tests for both core and dialect-specific features
- [x] Support error reporting with line/column information
- [x] Handle comments and whitespace
- [ ] Create `src/core/json_writer.f90` module for JSON serialization using json-fortran
- [ ] Add `to_json()` method to token type
- [ ] Create token serialization tests
- [ ] Benchmark lexer performance

### Test Cases
- [x] `test_lexer_basic.f90` - Basic tokenization (renamed from test_lexer_core_basic)
- [ ] `test_lexer_core_numbers.f90` - Standard number literal tokenization
- [ ] `test_lexer_core_operators.f90` - Standard operator tokenization
- [ ] `test_lexer_core_keywords.f90` - Standard keyword recognition
- [ ] `test_lexer_sf_basic.f90` - Simple Fortran specific features
- [ ] `test_lexer_errors.f90` - Error handling and recovery
- [ ] `test_lexer_serialization.f90` - Token to JSON serialization

## Phase 2: AST Definition - Core/Dialect Architecture

Define AST node types with shared core nodes and dialect extensions.

### Tasks
- [ ] Create `src/core/ast_core.f90` module with base node types
- [ ] Implement core node types shared by all Fortran dialects:
  - [ ] Base ast_node type with visitor pattern
  - [ ] Program node
  - [ ] Assignment node
  - [ ] Binary operation node
  - [ ] Function/subroutine definition
  - [ ] Function call node
  - [ ] Identifier node
  - [ ] Literal node
  - [ ] Use statement node
  - [ ] Print statement node
- [ ] Create `src/dialects/simple_fortran/ast_sf.f90` for extensions:
  - [ ] Extended program node with implicit program support
  - [ ] Type-inferred variable node
  - [ ] Future: List comprehension node
- [ ] Create `src/ast.f90` as unified interface
- [ ] Implement visitor pattern for AST traversal
- [ ] Write unit tests for AST construction

### Test Cases
- [ ] `test_ast_core_construction.f90` - Core AST node building
- [ ] `test_ast_sf_construction.f90` - Simple Fortran specific nodes
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

## Serialization Strategy

### Overview
All intermediate compilation stages will be serializable to JSON format for debugging, testing, and validation using the json-fortran library. Each test case will generate:

1. **Input**: `test_name.f` (Simple Fortran source)
2. **Tokens**: `test_name.tokens.json` (lexer output)
3. **AST**: `test_name.ast.json` (parser output - bare AST)
4. **Typed AST**: `test_name.typed.json` (after type inference)
5. **Annotated AST**: `test_name.annotated.json` (after all semantic analysis)
6. **Output**: `test_name.f90` (generated Fortran code)

### JSON Format Benefits
- Industry standard format with excellent tooling
- Human readable and editable
- Natural fit for tree structures like AST
- Easy to diff in version control
- Well-supported by json-fortran library (no manual implementation needed)
- Can be processed by external tools

### Example Formats

**Tokens** (simple_assignment.tokens.json):
```json
{
  "tokens": [
    {"type": "identifier", "text": "x", "line": 1, "column": 1},
    {"type": "operator", "text": "=", "line": 1, "column": 3},
    {"type": "number", "text": "5.0", "line": 1, "column": 5},
    {"type": "eof", "text": "", "line": 1, "column": 8}
  ]
}
```

**AST** (simple_assignment.ast.json):
```json
{
  "type": "program",
  "implicit": true,
  "body": [
    {
      "type": "assign",
      "target": {"type": "var", "name": "x"},
      "value": {"type": "literal", "kind": "real", "value": "5.0"}
    }
  ]
}
```

**Typed AST** (simple_assignment.typed.json):
```json
{
  "type": "program",
  "implicit": true,
  "body": [
    {
      "type": "assign",
      "target": {"type": "var", "name": "x", "datatype": {"kind": "real", "precision": 8}},
      "value": {"type": "literal", "kind": "real", "value": "5.0", "datatype": {"kind": "real", "precision": 8}}
    }
  ]
}
```

### Implementation Tasks
- [ ] Add `to_json()` method to token type using json-fortran
- [ ] Add `to_json()` visitor for AST nodes using json-fortran
- [ ] Create JSON writer wrapper module for consistent formatting
- [ ] Add serialization tests for each stage using json-fortran

## Success Criteria

1. All existing tests pass with new implementation
2. All examples work without modification
3. Performance is equal or better than current preprocessor
4. Code is more maintainable and extensible
5. Architecture supports future features (multiple dispatch, IR generation)
6. All intermediate stages are inspectable via JSON serialization

## Testing Strategy

- Write tests first (TDD approach)
- Each phase has dedicated unit tests
- Integration tests verify phase interactions
- Existing tests serve as regression suite
- Use `fpm test test_<subsystem>_*` for targeted testing
- Validate serialization/deserialization round trips
- Store golden outputs in test_data for regression testing

## Notes

- Reuse existing modules where possible (type inference, cache, etc.)
- Maintain backward compatibility throughout
- Focus on clean interfaces between phases
- Keep performance in mind from the start
- Document design decisions as we go
- Use JSON for all serialization (standard, well-supported)