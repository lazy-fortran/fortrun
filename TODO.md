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

## 🚧 IN PROGRESS: AST Implementation (doc/plan/AST.md)

### Current Status:
- **15/15 frontend tests passing** (100% pass rate) ✅
- **Working**: ALL frontend test cases including functions and nested calls
- **Next**: Stage 2 - Complete program unit support

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
- **Frontend tests**: 15/15 passing (100%) ✅
- **Full test suite**: ~75% passing
- **Key working**: ALL basic constructs (declarations, assignments, functions, nested calls)
- **Next focus**: Program units (modules, subroutines), type inference

### Target Milestones:
- **Stage 1**: ✅ COMPLETE - All frontend tests passing
- **Stage 2**: Program unit support (modules, subroutines, interfaces)
- **Stage 3**: Type inference restoration (Hindley-Milner)

## 🎯 IMMEDIATE PRIORITIES

### P0 (COMPLETED TODAY) ✅:
1. [x] Fixed function parameter duplicate declarations
2. [x] All frontend tests now passing (100%)
3. [x] Stage 1 complete

### P1 (THIS WEEK):
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