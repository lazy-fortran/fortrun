# TODO List for Fortran Frontend

## 🚨 CRITICAL: Function Parameter Type Inference (HIGH PRIORITY)

### Problem Statement
- Function parameters need proper type declarations with `intent(in)`
- Current AST structure doesn't support intent attributes on parameters
- Type information from semantic analysis isn't flowing to code generation
- Test expectations require `real(8), intent(in) :: x` format

### Detailed Implementation Plan

#### Phase 1: Extend AST Structure for Parameters
1. **Create parameter_declaration_node** (NEW NODE TYPE)
   - [ ] Add to ast_core.f90 with fields: name, type_name, kind_value, intent
   - [ ] Add JSON serialization support
   - [ ] Add visitor pattern support
   - [ ] Add factory function for creating parameter declarations

2. **Modify function_def_node structure**
   - [ ] Change param_indices to store parameter_declaration nodes instead of identifiers
   - [ ] Update parser to create parameter_declaration nodes
   - [ ] Ensure backward compatibility with existing code

#### Phase 2: Parser Enhancement
1. **Update parse_function_definition**
   - [ ] Parse parameter declarations with types (when present)
   - [ ] Create parameter_declaration nodes instead of identifier nodes
   - [ ] Handle both typed and untyped parameters for compatibility

2. **Add parse_parameter_declaration function**
   - [ ] Parse format: `type[(kind)] [, intent(in/out/inout)] :: name`
   - [ ] Support optional intent specification
   - [ ] Default to intent(in) for lowercase fortran

#### Phase 3: Semantic Analysis Integration
1. **Update analyze_function_def**
   - [ ] Store inferred parameter types in parameter_declaration nodes
   - [ ] Add type annotations to parameter nodes
   - [ ] Ensure type information is preserved in AST

2. **Create type annotation system**
   - [ ] Add type_annotation field to nodes that need it
   - [ ] Implement get/set methods for type annotations
   - [ ] Ensure annotations survive AST transformations

#### Phase 4: Standardizer Enhancement
1. **Update standardize_function_def**
   - [ ] Process parameter_declaration nodes
   - [ ] Apply inferred types to untyped parameters
   - [ ] Add intent(in) to parameters without intent
   - [ ] Generate proper parameter declarations in function body

2. **Implement standardize_function_parameters properly**
   - [ ] Create declaration nodes for each parameter
   - [ ] Use type information from semantic analysis
   - [ ] Insert after implicit none in function body
   - [ ] Handle array parameters correctly

#### Phase 5: Code Generation Update
1. **Update generate_code_declaration**
   - [ ] Add support for intent attribute
   - [ ] Format: `type(kind), intent(in) :: name`
   - [ ] Handle all intent types (in, out, inout)

2. **Update generate_code_function_def**
   - [ ] Generate parameters from parameter_declaration nodes
   - [ ] Ensure proper formatting of parameter list

## 🔧 Other High Priority Fixes

### Fix Standalone Function Wrapping
- [ ] Modify frontend.f90 to wrap standalone functions in program with contains
- [ ] Add logic to detect standalone function/subroutine definitions
- [ ] Generate appropriate program wrapper with contains statement
- [ ] Ensure module detection still works correctly

### Fix CLI JSON Options Test
- [ ] Debug --from-tokens execution failure
- [ ] Check JSON deserialization in frontend
- [ ] Ensure token stream reconstruction works
- [ ] Add better error messages for JSON parsing failures

### Fix Remaining Test Failures
- [ ] Fix example/fortran/step1_explicit_types/step1_demo.f
- [ ] Fix test_artifact_cache output file issues
- [ ] Review and fix remaining integration test failures

## 📋 Medium Priority Tasks

### Implement Select Case Statement
- [ ] Implement parse_select_case in parser_control_flow.f90
- [ ] Add case value parsing (single values, ranges, lists)
- [ ] Handle case default
- [ ] Add proper code generation for select case
- [ ] Test with various select case patterns

### Improve Error Handling
- [ ] Add location information to all error messages
- [ ] Implement error recovery in parser
- [ ] Add suggestions for common mistakes
- [ ] Create comprehensive error test suite

## 📊 Current Status

**Test Statistics:**
- Frontend tests: 26/29 passing (90%)
- Control flow: 100% working ✅
- Basic type inference: 100% working ✅
- Function type inference: Partial (missing parameter intent)

**Architecture Gaps:**
1. No parameter declaration node type in AST
2. No intent support in declaration nodes
3. Type information doesn't flow from semantic to codegen
4. Standalone functions aren't wrapped properly

## 🎯 Success Criteria

1. All 29 frontend tests passing (100%)
2. Function parameters have proper type and intent declarations
3. Standalone functions wrapped correctly
4. Type inference working end-to-end for all constructs

## 💡 Future Enhancements

- Full Fortran 2018 standard compliance
- Module and interface support
- Generic programming features
- Optimization passes in standardizer
- Better IDE integration support
