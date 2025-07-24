# TODO: Fix Documented but Non-Working Features

## Priority Classification
- **P0**: Core functionality blocking basic use cases
- **P1**: Major features that documentation claims work but don't
- **P2**: Important functionality gaps for project maturity
- **P3**: Nice-to-have features for completeness

---

## P0: Critical Frontend Pipeline (Blocking Everything)

### 1. Lexer Foundation
**Status**: 5 core lexer tests disabled, basic tokenization non-functional
**Tests to fix**: 
- `test_frontend_lexer_api.f90.disabled` - Core lexer API
- `test_frontend_lexer_keywords.f90.disabled` - Fortran keyword recognition
- `test_frontend_lexer_numbers.f90.disabled` - Numeric literal tokenization
- `test_frontend_lexer_operators.f90.disabled` - Operator tokenization

**TDD Approach**:
- Start with single integer literal: `42` → `INTEGER_LITERAL` token
- Add real literals: `3.14` → `REAL_LITERAL` token  
- Add identifiers: `x` → `IDENTIFIER` token
- Add operators: `+`, `-`, `*`, `/` → operator tokens
- Each test should verify exact token type and value

### 2. Basic Parser Core
**Status**: 20+ parser tests disabled, no working AST generation
**Tests to fix first**:
- `test_frontend_parser_api.f90.disabled` - Core parser interface
- `test_frontend_parser_basic.f90.disabled` - Simple expressions
- `test_frontend_parser_assignment.f90.disabled` - Variable assignments

**TDD Approach**:
- Parse single assignment: `x = 42` → Assignment AST node
- Parse arithmetic: `x = a + b` → Binary expression AST
- Parse function calls: `x = sin(y)` → Function call AST
- Each test verifies correct AST structure, not just "it doesn't crash"

### 3. Semantic Analysis Foundation  
**Status**: All 14 semantic tests broken, no type checking works
**Tests to fix**:
- `test_frontend_semantic_minimal.f90.broken` - Basic semantic context
- `test_frontend_semantic_basic_type_inference.f90.broken` - Integer/real inference

**TDD Approach**:
- Infer from literal: `x = 42` → `integer :: x`
- Infer from expression: `x = 3.14` → `real :: x`
- Infer from operation: `x = a + b` where `a,b` are integers → `integer :: x`
- Test actual type assignments, not just symbol table existence

---

## P1: Core Documented Features (Major Claims)

### 4. Array Type Inference
**Status**: README claims "✅ Type inference", but array tests broken
**Key broken test**: `test_frontend_semantic_inference_arrays.f90.broken`
**Documentation claim**: `data = [1, 2, 3, 4, 5]` → `integer :: data(5)`

**TDD Approach**:
- Parse array literal: `[1, 2, 3]` → ArrayLiteral AST
- Infer homogeneous types: `[1, 2, 3]` → `integer, dimension(3)`
- Infer from mixed compatible: `[1, 2.0]` → `real, dimension(2)`
- Reject incompatible: `[1, "hello"]` → type error
- Test actual array bounds and element types

### 5. Step 1 Type Enhancement
**Status**: Examples show `real function` → `real(8) function` but tests broken
**Key broken tests**: 
- `test_step1_integration.f90.broken`
- `test_step1_single_file.f90.broken`

**TDD Approach**:
- Basic type upgrade: `real x` → `real(8) :: x`
- Function return upgrade: `real function f()` → `real(8) function f()`
- Parameter intent: `subroutine s(x)` → `subroutine s(x)` with `intent(in)`
- Preserve existing declarations: `real(4) :: x` stays unchanged

### 6. Function Type Inference
**Status**: Documentation shows forward inference but tests broken
**Key test**: `test_frontend_semantic_function_type_inference.f90.broken`

**TDD Approach**:
- Return type inference: `f = sin(x)` where sin returns real → `real :: f`
- Parameter type inference: `call sub(42)` → parameter should be integer-compatible
- Chain inference: `x = f(y)` where `f` is defined elsewhere
- Complex calls: `x = g(f(y))` with nested function resolution

### 7. Control Flow Parsing
**Status**: Examples show do loops and if statements, but parser tests disabled
**Key tests**:
- `test_frontend_parser_do_loops.f90.disabled`
- `test_frontend_parser_if_statement.f90` (only one enabled)

**TDD Approach**:
- Simple do loop: `do i = 1, 10; end do` → LoopNode AST
- Do while: `do while (x > 0); end do` → WhileLoop AST  
- If-then: `if (x > 0) then; y = 1; end if` → IfNode AST
- If-else: Full if-then-else constructs with proper nesting

---

## P2: Integration and Advanced Features

### 8. Code Generation Pipeline
**Status**: All codegen tests disabled, no AST → Fortran conversion
**Key tests**: All in `test/frontend/codegen/*.disabled`

**TDD Approach**:
- Simple assignment codegen: Assignment AST → `integer :: x; x = 42`
- Expression codegen: Binary AST → `x = a + b`
- Function codegen: Function AST → proper function declaration
- Proper indentation and formatting in generated code

### 9. End-to-End Integration
**Status**: Integration tests disabled, no working pipeline
**Key test**: `test_parse_and_codegen.f90.disabled`

**TDD Approach**:
- Round-trip test: `.f` → AST → `.f90` → same semantic meaning
- Type preservation: Inferred types survive full pipeline
- Complex programs: Multi-statement programs work end-to-end
- Error propagation: Parse errors reported clearly to user

### 10. JSON Workflow Pipeline
**Status**: JSON workflow tests disabled
**Key tests**: 
- `test_json_workflow.f90.disabled`
- `test_json_pipeline.f90.broken`

**TDD Approach**:
- Token JSON: Source → JSON token stream → reconstructable
- AST JSON: Source → JSON AST → reconstructable AST
- Semantic JSON: Include type information in JSON output
- API consistency: JSON matches internal data structures

---

## P3: Advanced Language Features

### 11. Derived Types and Modules
**Status**: Advanced parser tests all disabled
**Key tests**: Multiple disabled tests for complex constructs

**TDD Approach**:
- Simple derived type: `type :: point; real :: x, y; end type`  
- Type member access: `p%x` where p is point type
- Module definitions: `module math; contains; end module`
- Use statements: `use math, only: sin` with proper scoping

### 12. Advanced Control Structures
**Status**: select case, complex loops disabled

**TDD Approach**:
- Select case: `select case (x); case (1); case default; end select`
- Nested loops: `do i = 1, n; do j = 1, m; end do; end do`
- Loop control: `exit`, `cycle` statements with proper scope
- Complex conditions: Multi-part logical expressions

### 13. Figure Capture (Currently WIP)
**Status**: README explicitly marks as work in progress

**TDD Approach**:
- Basic plot capture: `call plot(x, y)` → save figure file
- Multiple figures: Handle multiple plots in single program
- Format support: PNG, SVG output formats
- Integration: Notebook mode with embedded figures

---

## Test Quality Requirements

### Avoid Shallow/Tautological Tests
❌ **Bad**: `assert(parse_succeeded = .true.)` - just tests it didn't crash
✅ **Good**: `assert(ast%type == INTEGER_ASSIGNMENT .and. ast%value == 42)`

❌ **Bad**: `assert(infer_type() /= "")` - tests non-empty result  
✅ **Good**: `assert(infer_type("x = 3.14") == "real")`

### Fast, Independent Tests
- Each test runs in <100ms
- No file I/O unless testing file operations
- No dependencies between tests
- Clear test names: `test_infer_integer_from_literal()`

### Comprehensive Coverage
- Test success cases: normal valid input
- Test edge cases: empty input, boundary values  
- Test error cases: invalid syntax, type mismatches
- Test integration: components working together

---

## Implementation Strategy

1. **Start with P0**: Fix core lexer, parser, semantic foundation
2. **One test at a time**: Enable one disabled test, make it pass, move to next
3. **TDD cycle**: Red (failing test) → Green (minimal fix) → Refactor (clean up)
4. **No shortcuts**: Fix the actual functionality, don't just make tests pass
5. **Clean as you go**: Remove obsolete code, improve interfaces
6. **Small commits**: Each enabled test = one commit with clear message

The goal is to systematically close the gap between documentation claims and actual working functionality through rigorous test-driven development.