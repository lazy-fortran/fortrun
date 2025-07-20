# TODO List for Fortran Frontend

## 🚨 CRITICAL: Comprehensive Testing Strategy

### Testing Philosophy
**Every feature MUST have smart, non-shallow, non-tautological unit tests that:**
- Test actual behavior, not just syntax
- Cover edge cases and error conditions
- Verify semantic correctness, not just parsing
- Test integration between components
- Include regression tests for all bug fixes
- Test both positive and negative cases
- Verify type inference accuracy
- Check generated code correctness

### Testing Priorities
1. **Type Inference Tests**: Verify Algorithm W correctness with complex expressions
2. **Code Generation Tests**: Ensure generated Fortran compiles and runs correctly
3. **Error Recovery Tests**: Parser should handle malformed input gracefully
4. **Performance Tests**: Ensure compilation speed for large files
5. **Integration Tests**: Full pipeline from .f to executable

## ✅ Recently Completed (93% Test Success!)

### Major Accomplishments
- ✅ **Full JSON Round-Trip Implementation** - Complete serialization for all compilation phases
- ✅ Array parameter declarations with dimensions
- ✅ Derived type parameters with nested types
- ✅ Parser edge case handling
- ✅ Semantic analysis JSON output with type annotations
- ✅ Fixed frontend array bounds checking

### Function Handling Improvements
- ✅ Function parameter type inference with intent(in)
- ✅ Standalone function wrapping in programs
- ✅ Proper indentation for functions and their bodies
- ✅ Binary operator spacing standardization
- ✅ Fixed code duplication between CLI and compile_source API

### What's Working
- ✅ Basic type inference (Hindley-Milner Algorithm W)
- ✅ Control flow statements (if/else, do while)
- ✅ Function definitions and calls
- ✅ AST construction and transformation
- ✅ Code generation for most constructs
- ✅ JSON debugging and round-trip compilation

## 🔧 Remaining High Priority Tasks

### Fix Parameter Order Reversal
**Issue**: Parameters being reversed in code generation
```fortran
# Input:
function add_numbers(a, b)

# Output:
real(8), intent(in) :: b, a  # Wrong order!
```
**Root Cause**: Parameter collection iterates in reverse order
**Solution**: Fix parameter traversal in codegen_core.f90

### Implement Code Formatting Module
**Design a clean, reusable formatting module inspired by fprettify/findent**

#### Architecture:
```
src/frontend/formatting/
├── formatting_core.f90      # Core formatting interfaces and types
├── formatting_rules.f90     # Configurable formatting rules
├── formatting_indent.f90    # Indentation management
├── formatting_spacing.f90   # Whitespace and expression spacing
└── formatting_line.f90      # Line length and continuation handling
```

#### Key Interfaces:
```fortran
type :: format_options_t
    integer :: indent_width = 4
    integer :: max_line_length = 88
    logical :: space_around_operators = .true.
    logical :: group_declarations = .true.
    logical :: align_intent = .true.
end type

! Main formatting function
function format_code(code, options) result(formatted)
    character(len=*), intent(in) :: code
    type(format_options_t), intent(in) :: options
    character(len=:), allocatable :: formatted
end function

! Specific formatters
function format_expression(expr, options) result(formatted)
function format_declaration(decl, options) result(formatted)
function format_parameter_list(params, options) result(formatted)
```

#### Benefits:
- **Single Responsibility**: Each module handles one formatting aspect
- **DRY Principle**: Code generator uses formatting module, no duplication
- **Configurable**: Options for different formatting styles
- **Testable**: Each formatter can be unit tested independently
- **Extensible**: Easy to add new formatting rules

#### Integration Points:
1. **Code Generator**: Use formatters during code generation
2. **AST Visitors**: Apply formatting rules during traversal
3. **CLI Tool**: Standalone formatting command
4. **Pre-commit Hook**: Auto-format before commit

#### Implementation Tasks:
- [ ] Design format_options_t configuration type
- [ ] Implement expression spacing formatter
- [ ] Implement declaration grouping formatter
- [ ] Implement indentation manager
- [ ] Integrate with code generator
- [ ] Add comprehensive formatting tests

## 📋 Medium Priority Tasks

### Complete Phase 3: Advanced Semantic Analysis
- [ ] Persist type annotations through AST transformations
- [ ] Support polymorphic type inference
- [ ] Handle recursive function type inference
- [ ] Infer array dimensions and bounds
- [ ] **Test type inference with complex nested expressions**

### Implement Select Case Statement
- [ ] Parse case value lists and ranges
- [ ] Handle case default properly
- [ ] Generate optimized select case code
- [ ] **Test with all Fortran case patterns**

### Enhanced Error Handling
- [ ] Add column information to all errors
- [ ] Implement error recovery points
- [ ] Provide fix suggestions
- [ ] **Create error catalog with examples**

### Array Support Enhancement
- [ ] Infer array dimensions from usage
- [ ] Support array slicing operations
- [ ] Handle implicit array operations
- [ ] **Test multidimensional array inference**

## 🎯 Testing Requirements for Each Feature

### For Every New Feature:
1. **Unit Tests**: Test the feature in isolation
2. **Integration Tests**: Test with other features
3. **Error Tests**: Test error handling
4. **Performance Tests**: Ensure no regression
5. **Example Programs**: Real-world usage examples

### Test Coverage Goals:
- Line coverage: >90%
- Branch coverage: >85%
- Mutation testing: >75%
- All edge cases documented and tested

## 📊 Current Status

**Test Statistics:**
- Frontend tests: 27/29 passing (93%)
- Control flow: 100% working ✅
- Type inference: 100% working ✅
- Function handling: 95% working ✅
- JSON serialization: 100% working ✅

**Remaining Issues:**
1. Parameter order reversal (2 test failures)
2. Minor whitespace formatting differences
3. Some infrastructure test failures

## 💡 Future Enhancements (With Testing)

### Each Enhancement Must Include:
1. Comprehensive test suite BEFORE implementation
2. Benchmarks to measure improvement
3. Integration tests with existing features
4. Documentation with tested examples

### Planned Enhancements:
- Module and interface support (50+ tests needed)
- Generic programming (100+ tests needed)
- Coarray support (30+ tests needed)
- OpenMP/OpenACC directives (40+ tests needed)
- Optimization passes (performance test suite)

## 🏁 Definition of Done

A feature is ONLY complete when:
1. All tests pass (unit, integration, error cases)
2. Code coverage >90%
3. Performance benchmarks show no regression
4. Documentation includes tested examples
5. Edge cases are identified and tested
6. Error messages are helpful and tested

## 🚀 Next Sprint Goals

1. **Fix parameter order reversal** - Highest priority, affects 2 test cases
2. **Standardize code formatting** - Improve consistency
3. **Add missing test files** - Complete test coverage
4. **Improve error messages** - Better diagnostics
5. **Performance optimization** - Speed up compilation
