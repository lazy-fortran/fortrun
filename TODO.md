# TODO: Test Coverage Improvement Plan

## Current Status
- **Overall Coverage**: 17.95% (up from 14.08%)
- **Target Coverage**: 85% (ambitious but achievable)
- **Patch Coverage**: 0% (119 lines of new code uncovered)

## Coverage Gaps Analysis

### 1. Parser Expression Module (36 lines uncovered)
**File**: `src/frontend/parser/parser_expressions.f90`
**Priority**: HIGH - Core parsing functionality

**Missing Tests**:
- Array literal parsing `[1, 2, 3]`
- Error handling for malformed arrays `[1, 2,]`
- Expression precedence handling
- Parentheses in expressions
- Binary operators (arithmetic, logical, relational)
- Unary operators (`-x`, `+y`, `.not.`)
- Function call parsing

**Action Items**:
- [ ] Create `test_frontend_parser_expressions.f90` using arena-based API
- [ ] Test all operator types and precedence rules
- [ ] Test error cases for malformed expressions
- [ ] Ensure array literal edge cases are covered

### 2. Semantic Analyzer (24 lines uncovered)
**File**: `src/frontend/semantic/semantic_analyzer.f90`
**Priority**: HIGH - Type inference is critical

**Missing Tests**:
- Array literal type inference (`infer_array_literal` function)
- Mixed type promotion in arrays `[1, 2.0]` → `real`
- Function type inference
- Complex expression type inference

**Action Items**:
- [ ] Extend array literal tests with edge cases
- [ ] Add tests for type promotion scenarios
- [ ] Create tests for function return type inference
- [ ] Test nested expression type propagation

### 3. AST Core (20 lines uncovered)
**File**: `src/frontend/ast_core.f90`
**Priority**: MEDIUM - Infrastructure code

**Missing Tests**:
- New node types (array_literal_node)
- Node creation functions
- Arena management edge cases

**Action Items**:
- [ ] Test array literal node creation
- [ ] Test arena growth and memory management
- [ ] Test node access patterns

### 4. Code Generation (17 lines uncovered)
**File**: `src/frontend/codegen/codegen_core.f90`
**Priority**: MEDIUM - Output generation

**Missing Tests**:
- Array literal code generation `[1, 2, 3]`
- Complex nested structures
- Proper indentation handling

**Action Items**:
- [ ] Add array literal codegen tests
- [ ] Test deeply nested code structures
- [ ] Verify formatting consistency

### 5. Utilities (12 lines uncovered)
**File**: `src/utilities/temp_utils.F90`
**Priority**: LOW - Support code

**Missing Tests**:
- `fortran_with_isolated_cache` function
- `fortran_with_cache_dir` function
- Windows-specific paths

**Action Items**:
- [ ] Create utility function tests
- [ ] Test Windows path handling
- [ ] Test cache isolation

## Test Creation Strategy

### Phase 1: Parser Tests (Week 1)
1. **Expression Parser Test Suite**
   - Arithmetic: `a + b * c`, `(a + b) * c`
   - Logical: `x .and. y .or. z`
   - Relational: `a < b`, `x >= y`
   - Array literals: `[1, 2, 3]`, `[1.0, 2.0]`
   - Function calls: `sin(x)`, `max(a, b, c)`
   - Error cases: Invalid syntax

2. **Statement Parser Tests**
   - Assignments: `x = expr`
   - Declarations: `real :: x, y(10)`
   - Control flow already tested ✓

### Phase 2: Semantic Tests (Week 2)
1. **Type Inference Test Suite**
   - Literal inference: `42` → integer, `3.14` → real
   - Array inference: `[1, 2]` → integer(2)
   - Expression inference: `a + b` with known types
   - Function return types

2. **Scope and Symbol Tests**
   - Variable scoping
   - Function visibility
   - Module imports

### Phase 3: Integration Tests (Week 3)
1. **End-to-End Tests**
   - Parse → Analyze → Generate roundtrip
   - Complex multi-file programs
   - Error reporting and recovery

2. **Performance Tests**
   - Large file handling
   - Memory usage verification

## Coverage Improvement Tactics

### 1. Use Coverage Reports
```bash
# Generate coverage report
fpm test --coverage
# View uncovered lines
gcov -b src/frontend/parser/parser_expressions.f90
```

### 2. Focus on High-Impact Files
- Parser and semantic analyzer are most critical
- These handle core language features
- Higher coverage here = more reliable compiler

### 3. Test-Driven Development
- Write test first (red)
- Implement minimal code (green)
- Refactor and improve (clean)

### 4. Edge Case Coverage
- Empty inputs
- Maximum size inputs
- Invalid/malformed inputs
- Boundary conditions

## Success Metrics
- [ ] Patch coverage > 50% (60+ lines covered)
- [ ] Overall coverage > 25% (realistic near-term goal)
- [ ] All new features have tests
- [ ] No flaky/intermittent failures
- [ ] Tests run in < 30 seconds total

## Implementation Priority
1. **Immediate**: Parser expression tests (highest impact)
2. **This Week**: Array literal type inference tests
3. **Next Week**: Integration and roundtrip tests
4. **Future**: Advanced features and edge cases

The focus is on systematic improvement through well-designed tests that actually verify functionality, not just increase coverage numbers.