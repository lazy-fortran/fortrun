# TODO: Fix Documented but Non-Working Features

## KEY FINDINGS FROM INVESTIGATION

### ✅ What Actually Works
- **Lexer is robust and complete** - All 4 core lexer tests enabled and passing
- **Parser has modern arena-based architecture** - Some functionality working
- **Infrastructure is solid** - Module structure, caching, FPM integration work

### ❌ Real Blocking Issues  
- **Semantic Analysis is broken** - Module dependency hell prevents compilation
- **Type inference is non-functional** - This is what breaks the documented features
- **Build system complexity** - Standalone testing of semantic modules fails

### 📝 Updated Strategy
Focus on fixing the semantic analysis module dependencies rather than enabling every individual test. The lexer works fine, parser partially works, but semantic analysis is the real bottleneck.

---

## Priority Classification
- **P0**: Core functionality blocking basic use cases
- **P1**: Major features that documentation claims work but don't
- **P2**: Important functionality gaps for project maturity
- **P3**: Nice-to-have features for completeness

---

## P0: Critical Frontend Pipeline (Blocking Everything)

### 1. Lexer Foundation ✅ **COMPLETED**
**Status**: All 4 core lexer tests now ENABLED and PASSING
**Tests fixed**: 
- ✅ `test_frontend_lexer_api.f90` - Core lexer API (PASS)
- ✅ `test_frontend_lexer_keywords.f90` - Fortran keyword recognition (PASS)
- ✅ `test_frontend_lexer_numbers.f90` - Numeric literal tokenization (PASS)
- ✅ `test_frontend_lexer_operators.f90` - Operator tokenization (PASS)

**Key Finding**: The lexer implementation is robust and comprehensive. These tests were disabled unnecessarily - the lexer works perfectly for all basic Fortran constructs including keywords, numbers, operators, and identifiers.

### 2. Basic Parser Core ⚠️ **PARTIALLY WORKING**
**Status**: Parser has arena-based architecture and some functionality works
**Current working tests**: 
- ✅ `test_frontend_parser_if_statement.f90` - If statement parsing (PASS)
- ✅ Some parser functionality in frontend test group (PASS)

**Key Finding**: The parser has been modernized to use an arena-based AST system with indices rather than direct node references. Many disabled tests use the old API and need updating to new architecture.

**Real Issues**:
- Many disabled parser tests use obsolete API (`parse_statement` vs `parse_statement_dispatcher`)
- Tests need rewriting for arena-based AST system
- Focus should be on semantic analysis, not basic parsing

### 3. Semantic Analysis Foundation ❌ **CRITICAL ISSUE**
**Status**: All 14 semantic tests broken, this is the REAL blocking issue
**Root Cause**: Complex module dependency chain prevents compilation
**Tests to fix**:
- ⚠️ `test_frontend_semantic_minimal.f90` - Basic semantic context (ENABLED, build issues)
- `test_frontend_semantic_basic_type_inference.f90.broken` - Integer/real inference

**Key Finding**: The semantic analyzer module exists and looks comprehensive with Hindley-Milner type inference, but has complex dependencies (json_module, fpm integration) that prevent standalone testing.

**Priority**: This is where the real missing functionality lies. The lexer works, parser partially works, but semantic analysis is completely broken due to build/dependency issues.

---

## P1: Core Documented Features (Major Claims)

### 4. Array Type Inference ⚠️ **PARSER IMPLEMENTED**
**Status**: Array literal parsing now IMPLEMENTED in parser
**Progress**: 
- ✅ Added `LITERAL_ARRAY` constant to AST core
- ✅ Created `array_literal_node` AST node type  
- ✅ Implemented `push_array_literal` function in ast_factory
- ✅ Added array literal parsing `[1, 2, 3]` to `parse_primary` function
- ⚠️ Semantic analysis still needed for type inference

**Next Steps**:
- Enable semantic analysis to infer array types from literals
- Test array bounds inference: `[1, 2, 3]` → `integer, dimension(3)`
- Add mixed type handling: `[1, 2.0]` → `real, dimension(2)`

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