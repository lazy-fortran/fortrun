# TODO List for Fortran Frontend

## Immediate Fixes Required (Next Steps) 🚨

### Step 1: Fix Do While Loop Parsing (HIGH PRIORITY)
- [ ] Debug why do while loops generate "! Unparsed statement"
- [ ] Check parser_control_flow.f90 for do while implementation
- [ ] Ensure proper statement boundary detection for do while
- [ ] Fix statement parsing inside do while body
- [ ] Test with simple do while loop

### Step 2: Fix Select Case Statement Parsing (HIGH PRIORITY)  
- [ ] Debug incomplete select case structures
- [ ] Implement proper case branch parsing
- [ ] Handle case ranges (e.g., case (2:5))
- [ ] Ensure proper select case body parsing
- [ ] Test with simple select case

### Step 3: Update Test Expectations (HIGH PRIORITY)
- [ ] Update test_frontend_statements.f90 expected outputs
- [ ] Fix do while loop test expectations
- [ ] Update integration test expectations
- [ ] Ensure all test cases reflect new parser behavior

### Step 4: Function Code Generation Improvements (MEDIUM PRIORITY)
- [ ] Ensure proper function declaration handling
- [ ] Fix function parameter type preservation
- [ ] Handle function return types correctly
- [ ] Test function parsing and generation

### Step 5: Parser Cleanup and Optimization (LOW PRIORITY)
- [ ] Add better error messages for unparsed statements
- [ ] Improve parser error recovery
- [ ] Clean up debug output and logging

## Completed ✅

### MAJOR BREAKTHROUGH: Control Flow Parsing Fixed
- [x] **Fixed critical else-if nesting bug**: Parser no longer treats `else if` as nested if blocks
- [x] **3x parsing improvement**: control_flow_simple.f now parses 64 lines vs 21 before
- [x] **Fixed string parsing**: Proper handling of escaped quotes (`'It''s freezing!'`)
- [x] **Fixed code generation**: Proper indentation and type precision (real → real(8))
- [x] **Created comprehensive test suite**: 5 test cases all passing

### Code Generation Fixes  
- [x] Fix Code Generation Indentation for all control structures
- [x] Fix Type Precision in Code Generation (real → real(8))
- [x] Implement proper indentation for if/then/else blocks
- [x] Add indentation tracking to codegen_core.f90

### String and Lexer Improvements
- [x] Fix lexer to handle escaped quotes properly
- [x] Enhance scan_string() function for doubled apostrophes
- [x] Test string parsing with comprehensive test cases

### Control Flow Infrastructure
- [x] Fix control_flow_simple.f parsing (major improvement)
- [x] Debug and fix statement boundary detection
- [x] Fix if/else/elseif parsing sequence issues
- [x] Ensure all if blocks parse correctly

### Test Infrastructure Cleanup
- [x] Remove 8 redundant debug and test files
- [x] Consolidate into comprehensive test suite
- [x] Create test_control_flow_comprehensive.f90 with all scenarios
- [x] Follow cleanup policy: remove obsolete files

### Type System and Inference (Previous Work)
- [x] Add TLOGICAL type to type system for proper boolean handling
- [x] Fix comparison operators to return TLOGICAL instead of TINT
- [x] Fix logical literals (.true., .false.) to be typed as TLOGICAL
- [x] Update logical operations (.and., .or.) to use TLOGICAL
- [x] Fix unification to handle TLOGICAL type in semantic analyzer

### Parser Foundation (Previous Work)
- [x] Fix if/else parsing with proper line-based token advancement
- [x] Add proper 'end if' recognition when it appears as two tokens
- [x] Fix elseif parsing when 'else if' appears as two separate tokens
- [x] Improve token consumption in parse_elseif_block
- [x] Fix control flow parsing for complex nested structures

### Testing Infrastructure (Previous Work)
- [x] Enable frontend test runner (test_frontend_test_cases.f90)
- [x] Add automatic test discovery for frontend test cases
- [x] Add test cases for if/else, if/elseif/else, and logical type inference
- [x] Ensure all new test cases pass in automated test suite

### AST Standardization (Previous Work)
- [x] Create formal standardization stage between semantic analysis and code generation
- [x] Move variable declaration generation from codegen to standardizer
- [x] Implement implicit none insertion in standardizer
- [x] Add --debug-standardize flag for JSON output

## In Progress 🔄

### Parser Enhancements
- [ ] Fix do while loop parsing (highest priority)
- [ ] Fix select case statement parsing
- [ ] Add parser error context showing unparsed lines
- [ ] Improve error messages for parse failures

## Pending 📋

### Code Generation
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
- [ ] Add support for format statements
- [ ] Support array constructors and implied do loops
- [ ] Handle nested function calls and complex expressions

### Integration
- [ ] Fix remaining parsing issues in control_flow_simple.f
- [ ] Ensure all scientific examples compile and run correctly
- [ ] Update documentation with new features

## Known Issues 🐛

1. **Do while loops**: Generate "! Unparsed statement" instead of proper body
2. **Select case**: Incomplete structure generation
3. Some test failures due to updated parser behavior
4. Integration test dependencies and build issues

## Recent Major Success 🎉

**Parsing Coverage Improvement**: control_flow_simple.f parsing went from **26% (21/80 lines)** to **80% (64/80 lines)** - a dramatic improvement that demonstrates the core parsing infrastructure is now working correctly.

## Future Enhancements 💡

- Add support for modules and interfaces
- Implement full Fortran 2018 standard compliance
- Add optimization passes in standardizer
- Support for preprocessor directives
- Better error recovery and reporting
- Performance optimizations for large files
