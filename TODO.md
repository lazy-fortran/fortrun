# Full Type Inference Implementation

## Goal
Complete the lexer → parser → semantic analyzer → codegen pipeline for standardizing lazy fortran to Fortran 95, with full Hindley-Milner type inference using Algorithm W.

## Pipeline Architecture

The 4-phase compiler pipeline with JSON intermediate representations:

1. **Lexer**: Source Code → Tokens (outputs tokens.json with --debug-tokens)
2. **Parser**: Tokens → AST (inputs tokens via --from-tokens, outputs ast.json with --debug-ast)
3. **Semantic Analyzer**: AST → Annotated AST (inputs/outputs AST via --from-ast, adds type info with --debug-semantic)
4. **Code Generator**: AST → Fortran 95 code (inputs any AST via --from-ast)

Key insights:
1. The semantic analyzer augments the AST with type information but maintains the same structure
2. Both parser and semantic analyzer use the same JSON serialization (json_writer/json_reader modules)
3. The code generator accepts any AST via --from-ast, whether typed or untyped:
   - **Standard Fortran**: Parser → AST → Codegen (types already explicit)
   - **Lazy Fortran**: Parser → AST → Semantic Analysis → Annotated AST → Codegen (types inferred)

## Success Criteria
- [ ] All lazy fortran features compile to valid Fortran 95
- [ ] Double standardization test: Running standardizer on its own output produces identical results
- [ ] Complete test coverage for all Fortran 95 features via frontend_test_cases
- [ ] JSON intermediate representations work correctly at all stages
- [ ] No shortcuts - everything goes through the real AST pipeline

## Phase 1: Clean Up and Organize

### 1.1 Example Directory Cleanup ✅
- [x] Move all test snippets from example/ to example/frontend_test_cases/
- [x] Organize remaining examples into clean user-facing directories:
  - [x] example/basic/ - Simple getting started examples
  - [x] example/scientific/ - Scientific computing examples
  - [x] example/modules/ - Module usage examples
  - [x] example/lazy_fortran/ - Lazy fortran showcase examples

### 1.2 Verify Unified AST JSON Serialization ✅
- [x] Unified JSON modules already exist: json_writer.f90 and json_reader.f90
- [x] AST nodes have to_json method for serialization
- [x] json_reader supports deserialization of all AST node types
- [x] Verify parser uses json_writer for AST output
- [x] Verify semantic analyzer uses json_writer for AST output
- [x] Test: AST can round-trip through JSON (AST → JSON → AST)
- [x] Test: Semantic analyzer preserves all AST structure when adding type annotations

### 1.3 Remove Legacy Code ✅
- [x] Remove basic type inference code from frontend.f90
- [x] Remove any direct token-to-code shortcuts
- [x] Clean up temporary workarounds in codegen

## Phase 2: Lexer Enhancements

### 2.1 Complete Token Coverage ✅
- [x] Test: Integer literals (all forms) - Already implemented in test_frontend_lexer_numbers.f90
- [x] Test: Real literals (all forms including scientific notation) - Already implemented in test_frontend_lexer_numbers.f90  
- [x] Test: Complex literals - Already implemented in test_frontend_lexer_numbers.f90
- [x] Test: Character literals (single/double quotes) - Already implemented in test_frontend_lexer_api.f90
- [x] Test: Logical literals (.true./.false.) - Already implemented in test_frontend_lexer_api.f90
- [x] Test: All Fortran 95 operators - Already implemented in test_frontend_lexer_operators.f90
- [x] Test: All Fortran 95 keywords - Already implemented in test_frontend_lexer_keywords.f90

### 2.2 Lexer JSON Output Support ✅
- [x] Verify lexer outputs tokens.json with --debug-tokens flag - Already implemented and working
- [x] Test: Lexer correctly serializes all token types to JSON - Already implemented in test_frontend_lexer_serialization_comprehensive.f90

## Phase 3: Parser Enhancements

### 3.1 Statement Parsing
- [x] Test: Variable declarations (all types, attributes) ✅
- [x] Test: Array declarations (static, dynamic, assumed shape) ✅ - COMPLETE
  - [x] Static arrays: `real :: arr(10)` ✅
  - [x] Assumed shape arrays: `real :: arr(:)` ✅
  - [x] Multidimensional arrays: `real :: matrix(3, 4)` ✅
  - [x] Dynamic arrays with allocatable: `real, allocatable :: arr(:)` ✅
  - [x] Array bounds notation: `real :: arr(1:10)` ✅
  - [x] Character arrays with length: `character(len=20) :: names(5)` ✅
- [x] Test: Derived type definitions ✅
- [x] Test: Interface blocks ✅
- [x] Test: Module/contains structure ✅ - COMPLETE
- [x] Test: Use statements with renaming/only ✅ - COMPLETE
- [x] Test: Include statements ✅ - COMPLETE

### 3.2 Expression Parsing
- [x] Test: Arithmetic expressions (all operators, precedence) ✅
- [ ] Test: Logical expressions
- [ ] Test: Relational expressions
- [ ] Test: Array expressions and sections
- [ ] Test: Function calls (intrinsic and user-defined)
- [ ] Test: Structure member access

### 3.3 Control Flow
- [ ] Test: If/then/else/elseif
- [ ] Test: Select case
- [ ] Test: Do loops (all forms)
- [ ] Test: Where constructs
- [ ] Test: Forall constructs

### 3.4 Procedures
- [ ] Test: Function definitions
- [ ] Test: Subroutine definitions
- [ ] Test: Internal procedures
- [ ] Test: Module procedures
- [ ] Test: Generic interfaces
- [ ] Test: Operator overloading

### 3.5 Parser JSON Support
- [ ] Verify parser accepts tokens.json with --from-tokens flag
- [ ] Parser uses unified ast_json_io module for output
- [ ] Test: Parser correctly deserializes tokens and produces AST
- [ ] Test: AST serialization is consistent with semantic analyzer output

## Phase 4: Semantic Analysis (Type Inference)

### 4.1 Algorithm W Implementation
- [ ] Test: Basic type inference for assignments
- [ ] Test: Type inference for expressions
- [ ] Test: Function type inference
- [ ] Test: Array type inference
- [ ] Test: Polymorphic type handling
- [ ] Test: Type constraints and unification

### 4.2 Scope and Symbol Tables
- [ ] Test: Local variable scoping
- [ ] Test: Module scope handling
- [ ] Test: Use statement resolution
- [ ] Test: Interface resolution
- [ ] Test: Generic resolution

### 4.3 Type Checking
- [ ] Test: Assignment compatibility
- [ ] Test: Argument/parameter matching
- [ ] Test: Array conformance
- [ ] Test: Intrinsic function types
- [ ] Test: User-defined operators

### 4.4 Semantic AST Augmentation
- [ ] Verify semantic analyzer accepts ast.json with --from-ast flag
- [ ] Semantic analyzer uses unified ast_json_io module for input/output
- [ ] Test: Semantic analyzer adds type annotations to existing AST nodes
- [ ] Test: Augmented AST maintains same structure as parser output
- [ ] Test: Code generator works with both typed and untyped AST
- [ ] Test: AST with type annotations can be deserialized by codegen

## Phase 5: Code Generation

### 5.1 Declaration Generation
- [ ] Test: Generate type declarations from inferred types
- [ ] Test: Generate proper array declarations
- [ ] Test: Generate derived type declarations
- [ ] Test: Generate interface blocks

### 5.2 Statement Generation
- [ ] Test: Assignment statements
- [ ] Test: Control flow statements
- [ ] Test: Procedure calls
- [ ] Test: I/O statements

### 5.3 Program Structure
- [ ] Test: Generate proper program/end program
- [ ] Test: Generate contains section
- [ ] Test: Generate use statements
- [ ] Test: Generate implicit none

### 5.4 Double Standardization Test
- [ ] Test: For each frontend_test_case, standardize output again
- [ ] Test: Verify second standardization produces identical output

## Phase 6: Frontend Test Cases

### 6.1 Basic Features
- [ ] single_assignment: x = 1
- [ ] multiple_assignments: x = 1; y = 2.0
- [ ] arithmetic_ops: z = x + y * 2
- [ ] string_assignment: s = "hello"
- [ ] logical_assignment: flag = .true.
- [ ] array_literal: arr = [1, 2, 3]

### 6.2 Type Inference
- [ ] mixed_arithmetic: x = 1; y = x + 2.5
- [ ] function_inference: f(x) = x * 2; y = f(3.0)
- [ ] array_inference: arr = [1.0, 2.0]; x = arr(1)
- [ ] recursive_inference: fib(n) = if (n <= 1) then n else fib(n-1) + fib(n-2)

### 6.3 Control Flow
- [ ] if_statement: if (x > 0) then y = 1
- [ ] if_else: if (x > 0) then y = 1 else y = -1
- [ ] do_loop: do i = 1, 10; sum = sum + i; end do
- [ ] where_construct: where (arr > 0) arr = sqrt(arr)

### 6.4 Procedures
- [ ] simple_function: f(x) = x**2
- [ ] multi_arg_function: g(x, y) = x + y
- [ ] subroutine: subroutine swap(a, b)
- [ ] recursive_function: factorial(n)

### 6.5 Arrays
- [ ] array_declaration: real :: arr(10)
- [ ] dynamic_array: allocatable :: arr(:)
- [ ] array_operations: c = a + b
- [ ] array_sections: sub = arr(1:5)

### 6.6 Modules
- [ ] module_definition: module utils
- [ ] use_statement: use utils
- [ ] use_only: use utils, only: func
- [ ] use_renaming: use utils, func => my_func

### 6.7 Advanced Features
- [ ] derived_types: type point; real :: x, y; end type
- [ ] operator_overloading: interface operator(+)
- [ ] generic_interfaces: interface func
- [ ] parameterized_types: type :: matrix(n, m)

## Phase 7: Integration Testing

### 7.1 Full Pipeline Tests
- [ ] Test complete compilation of all frontend_test_cases
- [ ] Test with --standardize flag
- [ ] Test all debug output flags work correctly
- [ ] Test error handling and reporting

### 7.2 Performance Tests
- [ ] Benchmark compilation speed
- [ ] Memory usage profiling
- [ ] Cache effectiveness

## Phase 8: Documentation

### 8.1 Update Documentation
- [ ] Update CLAUDE.md with new architecture
- [ ] Document type inference algorithm
- [ ] Document JSON formats for each stage
- [ ] Update user examples

### 8.2 Error Messages
- [ ] Implement helpful type error messages
- [ ] Add source location tracking
- [ ] Provide type inference hints

## Implementation Order

1. Start with example cleanup (Phase 1)
2. Enhance lexer with full token support (Phase 2)
3. Build parser incrementally with tests (Phase 3)
4. Implement Algorithm W type inference (Phase 4)
5. Complete code generation (Phase 5)
6. Build comprehensive test suite (Phase 6)
7. Integration and performance testing (Phase 7)
8. Documentation (Phase 8)

## Testing Strategy

1. **Unit Tests First**: Test each component in isolation
2. **JSON Round-Trip**: Every stage must support JSON I/O
3. **Incremental Features**: One feature at a time
4. **Double Standardization**: Output must be idempotent
5. **No Shortcuts**: Everything through proper AST pipeline

## Current Status

**Phase 1 Complete ✅** - Example directory cleanup and legacy code removal finished.
**Phase 2 Complete ✅** - Lexer enhancements already implemented with comprehensive test coverage.

**Infrastructure Updates Complete ✅**:
- Added strict file creation discipline to CLAUDE.md
- Created `draft/` directory for temporary experimental work
- Updated .gitignore to prevent test artifact commits
- `temp_utils` linking issue resolved - test suite runs successfully

**CLI STDIN Support Complete ✅**:
- Implemented automatic STDIN detection (no args + piped input)
- Added handle_stdin_input() for temporary file creation from STDIN
- Modified parse_arguments to detect STDIN vs help display
- Fixed STDIN to default to lazy fortran (.f) extension for better user experience
- Full test coverage for STDIN functionality
- The fortran tool now supports Unix-style piping: `echo "x = 42" | fortran`

**Phase 3: Parser Enhancements - IN PROGRESS**

**Array Declaration Parsing Complete ✅**:
- Core array parsing infrastructure implemented with proper AST support
- Static arrays (`real :: arr(10)`), assumed shape (`real :: arr(:)`), and multidimensional arrays (`real :: matrix(3, 4)`) working
- Dynamic arrays with allocatable (`real, allocatable :: arr(:)`) implemented with attribute parsing
- Array bounds notation (`real :: arr(1:10)`) supported with enhanced dimension parsing
- Character arrays with length (`character(len=20) :: names(5)`) working with complex type specifications
- Extended declaration_node with is_array, dimensions, and is_allocatable fields
- JSON serialization support for array declarations
- Comprehensive test coverage for all implemented features

**Derived Type Definitions Complete ✅**:
- Simple derived type definitions: `type :: point` and `type point`
- Parameterized derived type definitions: `type :: matrix(n, m)` and `type matrix(n, m)`
- Variable declarations with derived types: `type(point) :: p`
- Array declarations with derived types: `type(point) :: points(10)`
- Enhanced parse_statement logic to distinguish derived type definitions from declarations
- Fixed declaration parser to handle derived types alongside traditional types
- All derived type parser tests passing

**Interface Blocks Complete ✅**:
- Added interface_block_node to AST with support for all interface types
- Implemented parse_interface_block function with proper keyword recognition
- Support for simple interfaces: `interface ... end interface`
- Support for generic interfaces: `interface name ... end interface`
- Support for operator interfaces: `interface operator(+) ... end interface`
- Support for assignment interfaces: `interface assignment(=) ... end interface`
- Added "interface" and "operator" keywords to lexer
- JSON serialization support for interface blocks
- Comprehensive test coverage for all interface block types

**Module/Contains Structure Complete ✅**:
- Added module_node to AST with name, declarations, procedures, and has_contains fields
- Implemented parse_module function recognizing module/contains/end module structure
- Support for simple modules: `module name ... end module`
- Support for modules with contains: `module name ... contains ... end module`
- Added "module" and "contains" keywords to lexer (now 30 keywords total)
- JSON serialization support for module structure
- Factory function create_module for consistent AST node creation
- Module name parsing and storage with proper line/column information
- Detection and flagging of contains section presence

**Use Statements with Renaming/Only Complete ✅**:
- Enhanced use_statement_node to support only clause and renaming
- Added only_list, rename_list, and has_only fields to AST
- Implemented parse_only_list function for parsing only clause items
- Support for basic use statements: `use module_name`
- Support for only clause: `use module_name, only: item1, item2`
- Support for renaming: `use module_name, only: new_name => old_name`
- Added "only" keyword to lexer (now 31 keywords total)
- JSON serialization support for only and rename lists
- Enhanced factory function create_use_statement with new parameters
- Comprehensive test coverage for all use statement forms

**Include Statements Complete ✅**:
- Added include_statement_node to AST with filename field
- Implemented parse_include_statement function for parsing include statements
- Support for basic include statements: `include 'filename.f90'`
- Added "include" keyword to lexer (now 32 keywords total)
- JSON serialization support for include statements
- Factory function create_include_statement for AST node creation
- JSON deserialization support in json_reader
- Comprehensive test coverage for include statement parsing

**Phase 3.1 Statement Parsing Complete ✅**

**Arithmetic Expressions Complete ✅**:
- Added comprehensive test coverage for arithmetic expressions
- All basic arithmetic operators working: +, -, *, /
- Operator precedence correctly implemented (multiplication before addition)
- Parentheses for expression grouping working
- Unary operators (+, -) supported
- Power operator (**) with correct precedence
- Existing expression parsing infrastructure is robust and complete

**Ready to proceed with Phase 3.2 Expression Parsing**
