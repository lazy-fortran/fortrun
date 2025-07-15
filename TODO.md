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

## 🚧 IN PROGRESS: AST Implementation (doc/plan/AST.md)

### Current Status:
- **9/15 frontend tests passing** (60% pass rate)
- **Working**: assignments, print statements, use statements, declarations
- **Failing**: function definitions, nested function calls

### Stage 1: Fix Function Parsing (ACTIVE)
**Problem**: Functions with parameters generate duplicate declarations

**Tasks**:
1. [ ] Fix duplicate parameter declarations in functions
2. [ ] Handle function parameter vs body declaration distinction
3. [ ] Update expected outputs for function tests

### Stage 2: Complete Program Unit Support
**Goal**: Full support for all Fortran program units

**Tasks**:
1. [ ] Implement subroutine parsing
2. [ ] Implement module parsing
3. [ ] Handle contains sections properly
4. [ ] Support interface blocks

### Stage 3: Re-enable Type Inference
**Goal**: Restore Hindley-Milner type system

**Tasks**:
1. [ ] Fix type variable substitution crashes
2. [ ] Implement function type inference
3. [ ] Add array type support
4. [ ] Handle user-defined types

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
- **Frontend tests**: 9/15 passing
- **Full test suite**: ~65% passing
- **Key working**: declarations, assignments, print
- **Key broken**: functions, type inference

### Target Milestones:
- **Stage 1**: 12/15 frontend tests (fix functions)
- **Stage 2**: 15/15 frontend tests (all constructs)
- **Stage 3**: 100% test suite (type inference restored)

## 🎯 IMMEDIATE PRIORITIES

### P0 (TODAY):
1. [ ] Fix function parameter duplicate declarations
2. [ ] Update function test expected outputs
3. [ ] Get nested_function_calls test passing

### P1 (THIS WEEK):
1. [ ] Complete Stage 1 (function parsing)
2. [ ] Start Stage 2 (program units)
3. [ ] Maintain test coverage above 80%

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