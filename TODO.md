# TODO List for Fortran Frontend

## Immediate Fixes Required (Step by Step) 🚨

### Step 1: Fix Code Generation Indentation
- [ ] Add indentation tracking to codegen_core.f90
- [ ] Implement proper indentation for if/then/else blocks
- [ ] Implement proper indentation for contains blocks
- [ ] Implement proper indentation for function/subroutine bodies
- [ ] Update expected test outputs with correct indentation

### Step 2: Fix Type Precision in Code Generation
- [ ] Preserve real(8) instead of converting to real
- [ ] Preserve intent(in) attributes for parameters
- [ ] Generate all parameter declarations in functions
- [ ] Update single_real_declaration test expectations

### Step 3: Fix control_flow_simple.f Parsing
- [ ] Debug why parser stops after first if block
- [ ] Check if issue is in parse_statement_dispatcher
- [ ] Ensure all top-level statements are parsed
- [ ] Add debug output to trace parsing flow
- [ ] Fix any issues with statement sequence parsing

### Step 4: Fix Function Code Generation
- [ ] Ensure contains keyword is properly indented
- [ ] Add implicit none to generated functions
- [ ] Generate proper function signatures with types
- [ ] Handle multiple parameters correctly

### Step 5: Update Test Expectations
- [ ] Update if_else_simple expected output with indentation
- [ ] Update if_elseif_else expected output with indentation
- [ ] Update function test cases with proper formatting
- [ ] Ensure all test cases have required input/output files

## Completed ✓

### Type System and Inference
- [x] Add TLOGICAL type to type system for proper boolean handling
- [x] Fix comparison operators to return TLOGICAL instead of TINT
- [x] Fix logical literals (.true., .false.) to be typed as TLOGICAL
- [x] Update logical operations (.and., .or.) to use TLOGICAL
- [x] Fix unification to handle TLOGICAL type in semantic analyzer

### Parser Improvements
- [x] Fix if/else parsing with proper line-based token advancement
- [x] Add proper 'end if' recognition when it appears as two tokens
- [x] Fix elseif parsing when 'else if' appears as two separate tokens
- [x] Improve token consumption in parse_elseif_block
- [x] Fix control flow parsing for complex nested structures

### Testing Infrastructure
- [x] Enable frontend test runner (test_frontend_test_cases.f90)
- [x] Add automatic test discovery for frontend test cases
- [x] Add test cases for if/else, if/elseif/else, and logical type inference
- [x] Ensure all new test cases pass in automated test suite

### AST Standardization
- [x] Create formal standardization stage between semantic analysis and code generation
- [x] Move variable declaration generation from codegen to standardizer
- [x] Implement implicit none insertion in standardizer
- [x] Add --debug-standardize flag for JSON output

## In Progress 🔄

### Parser Enhancements
- [ ] Add parser error context showing unparsed lines
- [ ] Improve error messages for parse failures

## Pending 📋

### Code Generation
- [ ] Fix indentation in generated code (proper nesting for if/else blocks)
- [ ] Enable and fix disabled codegen unit tests
- [ ] Add support for more complex control structures

### Frontend Tests
- [ ] Enable and fix other disabled frontend tests
- [ ] Add more comprehensive test cases for edge cases
- [ ] Test error handling and recovery

### Standardizer Enhancements
- [ ] Add module transformation support to standardizer
- [ ] Implement program vs module decision logic in standardizer
- [ ] Handle contains statement insertion for modules
- [ ] Support more complex AST transformations

### Type System Extensions
- [ ] Add support for derived types
- [ ] Implement type inference for array operations
- [ ] Handle implicit type conversions

### Parser Extensions
- [ ] Add support for select case statements
- [ ] Implement do while loop parsing
- [ ] Handle format statements
- [ ] Support array constructors and implied do loops

### Integration
- [ ] Fix remaining issues in control_flow_simple.f example
- [ ] Ensure all scientific examples compile and run correctly
- [ ] Update documentation with new features

## Known Issues 🐛

1. Some complex files like control_flow_simple.f stop parsing partway through
2. Single_real_declaration test expects real(8) but gets real
3. Some whitespace sensitivity in test comparisons
4. Long lines in generated code (exceeding 88 character limit)

## Future Enhancements 💡

- Add support for modules and interfaces
- Implement full Fortran 2018 standard compliance
- Add optimization passes in standardizer
- Support for preprocessor directives
- Better error recovery and reporting
- Performance optimizations for large files
