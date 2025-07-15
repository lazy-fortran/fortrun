# *Lazy Fortran* Compiler Frontend TODO

## ✅ COMPLETED PHASES

### Phase 0-13: Foundation Complete
- Basic architecture, lexer, parser foundations
- Cache system with module conflict resolution
- Organized test infrastructure

### Phase 8: Dialect-Agnostic AST with JSON Workflow
- Dialect-agnostic core modules (codegen_core, debug_utils, semantic_analyzer)
- Unified type inference through core assignment_node
- Complete JSON workflow (--from-tokens, --from-ast, --from-semantic)
- JSON deserializer made dialect-agnostic
- Full CLI integration and test coverage

### Recent Fixes (AST Branch):
- Fixed lexer token accumulation issues
- Fixed parser program unit boundary detection
- Implemented verbose level 3 (-vvv) logging system
- Fixed duplicate unnamed functions in multi-function files
- **Implemented declaration node parsing** - standalone declarations now properly handled
- Added automatic test discovery for frontend test cases
- Parser now creates proper AST nodes for declarations (not empty literals)
- **Fixed duplicate parameter declarations** - functions no longer generate redundant declarations

### Recent Discoveries (2025-01-15):
- **Type Inference Status**: Frontend uses FALLBACK system, not Hindley-Milner
- **Fallback System**: `declaration_generator` module with `infer_basic_type`
  - Infers types from literal values in AST
  - Binary ops inherit type from left operand
  - Identifiers/functions default to real(8)
- **Semantic Analyzer**: SKIPPED for programs with function calls (workaround)
- **ast_extensions**: Stub module that caused gfortran 14.1.0 segfault
- **Why Tests Pass**: Fallback type inference generates working code

## 🚧 IN PROGRESS: AST Implementation (doc/plan/AST.md)

### Current Status:
- **15/15 frontend tests passing** (100% pass rate) ✅
- **Working**: ALL frontend test cases including functions, nested calls, select case, unary operators
- **Critical fixes completed**: Select case, intrinsic functions, registry tests all passing
- **Next**: Debug control_flow_simple.f segfault, then Stage 2 - Complete program unit support

### Stage 1: Fix Function Parsing (COMPLETED) ✅
- Fixed duplicate parameter declarations in functions
- Handled function parameter vs body declaration distinction
- All frontend test cases now pass

### Stage 2: Complete Program Unit Support
**Goal**: Full support for all Fortran program units

**Tasks**:
1. [ ] Implement subroutine parsing
2. [ ] Implement module parsing
3. [ ] Handle contains sections properly
4. [ ] Support interface blocks

### Stage 3: Replace Basic Type Inference with Semantic Analysis
**Goal**: Replace fallback type inference with proper Hindley-Milner + AST annotations

**Current State**:
- Frontend uses `declaration_generator` module with `infer_basic_type` (fallback system)
- Semantic analyzer with Hindley-Milner is SKIPPED for programs with function calls
- ast_extensions module was stub code that crashed gfortran 14.1.0
- Type information is NOT stored in AST nodes currently

**Step-by-Step Plan**:

#### Phase 3.1: Analysis Only (No AST Modification)
1. [ ] Fix semantic analyzer crashes for function calls
   - Debug type variable substitution in `unify_types`
   - Handle function type inference properly
   - Ensure semantic analysis completes without modifying AST

2. [ ] Verify semantic analysis produces correct types
   - Add debug output to show inferred types
   - Compare with fallback `infer_basic_type` results
   - Create test cases for type inference validation

#### Phase 3.2: AST Extension Design
3. [ ] Design proper AST annotation mechanism
   - Option A: Add `inferred_type` field to base `ast_node`
   - Option B: Create separate type annotation table (like ast_extensions tried)
   - Option C: Create typed wrapper nodes (e.g., `typed_assignment_node`)
   - Choose based on gfortran compatibility and memory efficiency

4. [ ] Implement chosen AST annotation mechanism
   - Ensure no gfortran crashes (test with 14.1.0)
   - Keep modifications minimal and focused
   - Maintain AST immutability where possible

#### Phase 3.3: Integration
5. [ ] Modify semantic analyzer to annotate AST
   - After successful type inference, store type in AST node
   - Handle all node types systematically
   - Ensure annotation doesn't break existing functionality

6. [ ] Update codegen to use annotated types
   - Modify `generate_declarations` to read from AST annotations
   - Remove dependency on `infer_basic_type` fallback
   - Ensure all declaration generation uses semantic analysis results

7. [ ] Remove fallback system
   - Delete `declaration_generator` module
   - Remove `contains_function_calls` workaround in frontend.f90
   - Always run semantic analysis for all programs

**Critical Requirements**:
- Analysis phase MUST complete successfully before any AST modification
- AST modifications MUST be validated to not crash gfortran
- Type information MUST be accessible to codegen without circular dependencies

## ⚠️ CRITICAL ARCHITECTURAL REQUIREMENTS

### Parser Architecture:
- **MUST** parse program units, not individual lines
- **MUST** recognize multi-line constructs (functions, modules)
- **MUST** follow Fortran 95 standard structure

### AST Requirements:
- **NO** direct token-to-code shortcuts
- **ALL** code generation through complete AST pipeline
- **ALL** type information from semantic analysis

### Lazy Fortran Semantic Analysis:
- **TODO**: Implement deferred program/module decision
- Parser must be permissive (allow both contexts)
- Semantic analyzer decides based on content
- File basename becomes program/module name

## 📊 TEST METRICS

### Current (AST Branch):
- **Frontend tests**: 15/15 passing (100%) ✅
- **Full test suite**: ~75% passing
- **Key working**: ALL basic constructs (declarations, assignments, functions, nested calls)
- **Next focus**: Program units (modules, subroutines), type inference

### Target Milestones:
- **Stage 1**: ✅ COMPLETE - All frontend tests passing
- **Stage 2**: Program unit support (modules, subroutines, interfaces)
- **Stage 3**: Type inference restoration (Hindley-Milner)

## 🎯 IMMEDIATE PRIORITIES

### P0 (CRITICAL - FIX FAILING TESTS):
1. [x] Fix select case statement parsing/codegen ✅
   - Implemented full select case parsing with case blocks
   - Added range support (2:5 syntax) for case values
   - Updated frontend to handle multi-line select case constructs
2. [x] Fix intrinsic function handling ✅
   - Fixed unary operator parsing (-5.5 syntax)
   - abs() function now works correctly
   - Added support for unary + and - operators
3. [x] Fix registry enhancement test ✅
   - Added verbose output for module resolution
   - Test now passes with proper package name output

### P0.5 (REMAINING CRITICAL):
1. [x] Debug control_flow_simple.f segfault ✅
   - Root cause: do while loops are not implemented in parser
   - Added temporary workaround to prevent segfault
   - do while loops now need proper implementation

### P1 (THIS WEEK - AFTER FIXES):
1. [ ] Start Stage 2 - Module parsing support
2. [ ] Add subroutine parsing
3. [ ] Handle interface blocks

### P2 (NEXT WEEK):
1. [ ] Complete Stage 2
2. [ ] Begin Stage 3 (type inference)
3. [ ] Full frontend test suite passing

## 💡 KEY INSIGHTS

### What's Working Well:
- Clean separation of lexer/parser/semantic/codegen
- JSON intermediate representations for debugging
- Comprehensive test infrastructure
- Verbose logging for debugging

### What Needs Work:
- Function body parsing (parameter handling)
- Multi-line construct recognition
- Type inference stability
- Error recovery and diagnostics

**GOAL**: Complete AST implementation with all frontend tests passing, building toward a production-ready lazy fortran compiler.