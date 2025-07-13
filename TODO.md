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

## Phase 1: Lexer Implementation - Core/Dialect Architecture ✅

Build a tokenizer with shared core functionality and dialect-specific extensions.

### Tasks
- [x] Create directory structure: `src/core/`, `src/dialects/simple_fortran/`
- [x] Create `src/core/lexer_core.f90` module with standard Fortran tokenization
- [x] Define base token types shared by all dialects
- [x] Create `src/lexer.f90` as main interface that delegates to appropriate implementation
- [x] Write unit tests for both core and dialect-specific features
- [x] Support error reporting with line/column information
- [x] Handle comments and whitespace
- [x] Create JSON serialization using json-fortran
- [x] Add `to_json()` method to token type
- [x] Create token serialization tests

### Test Cases
- [x] `test_lexer_basic.f90` - Basic tokenization
- [x] `test_lexer_numbers.f90` - Number literal tokenization
- [x] `test_lexer_operators.f90` - Operator tokenization
- [x] `test_lexer_keywords.f90` - Keyword recognition
- [x] `test_lexer_errors.f90` - Error handling and recovery
- [x] `test_lexer_serialization.f90` - Token to JSON serialization

## Phase 2: AST Definition - Core/Dialect Architecture ✅

Define AST node types with shared core nodes and dialect extensions.

### Tasks
- [x] Create `src/core/ast_core.f90` module with base node types
- [x] Implement core node types shared by all Fortran dialects:
  - [x] Base ast_node type with visitor pattern
  - [x] Program node
  - [x] Assignment node
  - [x] Binary operation node
  - [x] Function/subroutine definition
  - [x] Function call node
  - [x] Identifier node
  - [x] Literal node
  - [x] Use statement node
  - [x] Print statement node
- [x] Create `src/dialects/simple_fortran/ast_sf.f90` for extensions:
  - [x] Extended program node with implicit program support
  - [x] Type-inferred variable node
  - [x] Future: List comprehension node
  - [x] Future: F-string node
  - [x] Enhanced assignment node with type inference metadata
- [x] Create `src/ast.f90` as unified interface
- [x] Implement visitor pattern infrastructure for AST traversal
- [x] Write comprehensive unit tests for AST construction
- [x] Implement JSON serialization for all AST nodes using json-fortran
- [x] Factory functions with proper polymorphic array allocation

### Test Cases
- [x] `test_ast_construction.f90` - Core and Simple Fortran AST node building, JSON serialization

## Phase 3: Parser Implementation ✅

Build recursive descent parser for Simple Fortran using **TDD approach**.

### TDD Implementation Strategy
1. **Write failing tests first** that define expected parser behavior
2. **Implement minimal parsing logic** to make tests pass
3. **Refactor** for better design while keeping tests green
4. **Repeat incrementally** for each parsing feature

### Tasks
- [x] Create `src/core/parser_core.f90` module with base parsing functionality
- [x] Create `src/parser.f90` as unified interface
- [x] Implement expression parsing with precedence handling
- [x] Implement statement parsing (assignments)
- [x] Integration with existing lexer
- [ ] Handle implicit program wrapping for Simple Fortran
- [ ] Support function/subroutine definitions
- [ ] Error recovery and detailed error reporting
- [ ] JSON serialization of parse trees

### Test Cases (TDD Order)
- [x] `test_parser_basic.f90` - Basic expression parsing
- [x] `test_parser_binary_ops.f90` - Binary operations with precedence
- [ ] `test_parser_statements.f90` - Statement parsing (assignments, prints)
- [ ] `test_parser_functions.f90` - Function/subroutine parsing
- [ ] `test_parser_programs.f90` - Full program parsing with implicit wrapping
- [ ] `test_parser_errors.f90` - Error recovery and reporting
- [ ] `test_parser_serialization.f90` - AST to JSON serialization

## Phase 5: AST-Based Preprocessor Integration ✅

Replace line-based preprocessor with AST-based approach.

### Tasks
- [x] Create `src/preprocessor_ast.f90` module
- [x] Implement basic type inference for literals
- [x] Line-by-line processing for .f files
- [x] Generate variable declarations
- [x] Integrate with runner (default for .f files)
- [x] Add environment variable control (FORTRAN_USE_AST_PREPROCESSOR)
- [ ] Support functions and subroutines
- [ ] Support arrays and derived types
- [ ] Advanced type inference

### Test Cases
- [x] `test_preprocessor_ast.f90` - Basic AST preprocessor tests
- [ ] `test_preprocessor_ast_functions.f90` - Function preprocessing
- [ ] `test_preprocessor_ast_arrays.f90` - Array preprocessing

### Known Limitations
- Arrays not yet supported
- Derived types not yet supported
- Functions/subroutines not yet supported
- Comments and markdown cells not preserved

## Phase 4: Code Generation ✅

Generate Fortran code from AST.

### Tasks
- [x] Create `src/core/codegen_core.f90` module
- [x] Implement AST to Fortran transformation
- [x] Handle basic node types (literal, identifier, assignment, binary_op)
- [x] Generate program structure
- [ ] Apply modern defaults (real(8), etc.)
- [ ] Handle indentation and formatting
- [ ] Generate contains statements

### Test Cases
- [x] `test_codegen_basic.f90` - Basic code generation
- [x] `test_codegen_expressions.f90` - Expression code generation
- [x] `test_codegen_program.f90` - Program generation
- [x] `test_parse_and_codegen.f90` - Round-trip parsing and generation
- [ ] `test_codegen_functions.f90` - Function generation
- [ ] `test_codegen_defaults.f90` - Modern defaults application
- [ ] `test_codegen_formatting.f90` - Code formatting

## Phase 6: Cache Management Enhancement ✅

Improve cache management with command-line support.

### Tasks
- [x] Add `--clear-cache` option to CLI
- [x] Add `--cache-info` option to show cache statistics
- [x] Implement cache clearing functionality
- [x] Update help text with new options
- [x] Add tests for cache management commands
- [x] Update documentation with cache management examples

### Test Cases
- [x] `test_cli_cache.f90` - Test cache clearing options

### Usage Examples
```bash
# Clear all cache
fortran --clear-cache

# Clear cache and run
fortran --clear-cache example.f90

# Show cache information
fortran --cache-info
```

### Implementation Details
- Cache info shows: directory path, file count, total size
- Clear cache removes all files but preserves directory structure
- Clear cache with filename clears cache then runs the file
- Works with custom cache directories via --cache-dir

## Phase 7: Advanced Features

Extend AST preprocessor with advanced features.

### Tasks
- [ ] Support function/subroutine definitions
- [ ] Support array type inference
- [ ] Support derived types
- [ ] Preserve comments and markdown cells
- [ ] Advanced expression type inference
- [ ] Symbol table and scope management

## Phase 8: Full Integration

Complete replacement of legacy preprocessor.

### Tasks
- [ ] Ensure all examples work with AST preprocessor
- [ ] Remove legacy preprocessor code
- [ ] Update all documentation
- [ ] Performance optimization
- [ ] Remove FORTRAN_USE_AST_PREPROCESSOR environment variable

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
7. **Implementation follows strict TDD (red-green-refactor) cycle**

## Testing Strategy

- **Write tests first (TDD approach)** - failing tests define expected behavior
- **Minimal implementation** to make tests pass
- **Refactor** while keeping tests green
- **Incremental development** - one feature at a time
- Each phase has dedicated unit tests
- Integration tests verify phase interactions
- Existing tests serve as regression suite
- Use `fpm test test_<subsystem>_*` for targeted testing
- Validate serialization/deserialization round trips
- Store golden outputs in test_data for regression testing

## Progress Summary

### ✅ **Completed Phases**
- **Phase 0**: Test Reorganization - Hierarchical test structure with FPM wildcards
- **Phase 1**: Lexer Implementation - Core tokenizer with JSON serialization
- **Phase 2**: AST Definition - Comprehensive node types with visitor pattern and JSON output
- **Phase 3**: Parser Implementation - Recursive descent parser for expressions and statements
- **Phase 4**: Code Generation - AST to Fortran transformation
- **Phase 5**: AST-Based Preprocessor - Basic integration with type inference
- **Phase 6**: Cache Management Enhancement - CLI options for cache control

### 🚧 **Current Phase**
- **Phase 7**: Advanced Features - Functions, arrays, derived types for AST preprocessor

### 📋 **Upcoming Phases**
- **Phase 7**: Advanced Features - Functions, arrays, derived types
- **Phase 8**: Full Integration - Complete legacy preprocessor replacement

## Important Testing Notes

⚠️ **ALWAYS clear the cache before testing new features!**

The fortran tool aggressively caches preprocessed files. When developing or testing new preprocessor features:

```bash
# Clear cache manually
rm -rf ~/.cache/fortran/*

# Or use the upcoming command (Phase 6)
fortran --clear-cache

# Then run your test
fortran example.f90
```

## Notes

- Reuse existing modules where possible (type inference, cache, etc.)
- Maintain backward compatibility throughout
- Focus on clean interfaces between phases
- Keep performance in mind from the start
- Document design decisions as we go
- Use JSON for all serialization (standard, well-supported)