# AST Architecture for *Lazy Fortran* Compiler Frontend

## Current Implementation Status ✅

**COMPLETED**: The AST-based compiler frontend is **production-ready** with a complete 4-phase architecture.

### Architecture Overview

The frontend implements a clean, modular pipeline following modern compiler design principles:

```
Source Code (.f, .f90)
        ↓
┌──────────────────────────────────────┐
│        Production Frontend           │
│                                      │
│  ┌────────────┐                     │
│  │   Lexer    │ → Tokens (JSON)     │
│  └────────────┘                     │
│         ↓                           │
│  ┌────────────┐                     │
│  │   Parser   │ → AST (JSON)        │
│  └────────────┘                     │
│         ↓                           │
│  ┌────────────────────┐             │
│  │ Semantic Analyzer  │ → Typed AST │
│  │ (Hindley-Milner)   │    (JSON)   │
│  └────────────────────┘             │
│         ↓                           │
│  ┌────────────────────┐             │
│  │  Code Generator    │ → F90 Code  │
│  └────────────────────┘             │
│                                      │
│  Debug: --debug-tokens --debug-ast   │
│         --debug-semantic --debug-    │
│         codegen                      │
└──────────────────────────────────────┘
        ↓
   Standard Fortran 90 Output
```

## Core Components

### 1. Lexer (`src/frontend/lexer/lexer_core.f90`)
- **Dialect-agnostic tokenization**
- 24 Fortran keywords, 8 token types
- JSON serialization for debug/testing
- Handles operators, numbers, identifiers, strings

### 2. Parser (`src/frontend/parser/parser_core.f90`)
- **Dialect-agnostic recursive descent parser**
- Comprehensive AST node generation
- Handles expressions, statements, functions, control flow
- Proper multi-line construct parsing (functions, do loops, if blocks)

### 3. AST Core (`src/frontend/ast_core.f90`)
- **Complete AST node hierarchy**
- Visitor pattern support
- Polymorphic array wrapper pattern (gfortran 15 compatible)
- Source location preservation

### 4. Semantic Analyzer (`src/frontend/semantic/semantic_analyzer.f90`)
- **Hindley-Milner type inference with Algorithm W**
- Full unification and substitution system
- Type schemes and generalization
- Occurs check and type safety

### 5. Code Generator (`src/frontend/codegen/codegen_core.f90`)
- **Standard Fortran 90 output generation**
- Modern defaults (implicit none, real(8))
- Proper program structure generation
- Intent(in) parameter enhancement

## Architecture Principles

### 1. Dialect Agnostic Core
- Core modules (`lexer_core`, `parser_core`, `semantic_analyzer`, `codegen_core`, `ast_core`) contain **NO** dialect-specific features
- All extensions go in `src/frontend/standard/` subdirectories
- Clean separation between standards and implementation

### 2. Complete AST Pipeline
- **MANDATORY**: All processing through proper AST nodes
- **FORBIDDEN**: Direct token-to-code shortcuts
- **REQUIRED**: Full semantic analysis for all constructs
- **ENFORCED**: No string manipulation bypassing AST

### 3. JSON Workflow
Each phase supports JSON input/output for debugging and testing:
- `--debug-tokens` → tokens.json
- `--debug-ast` → ast.json  
- `--debug-semantic` → semantic.json
- `--debug-codegen` → codegen.json

### 4. Type System
- **Hindley-Milner Algorithm W** for automatic inference
- **Type variables** with proper occurs checking
- **Substitution system** with composition
- **Polymorphic wrapper pattern** for array safety

## Key Design Decisions

### 1. Polymorphic Array Handling
**Problem**: `class(ast_node), allocatable :: array(:)` causes allocation errors in gfortran 15.

**Solution**: Wrapper pattern
```fortran
type :: ast_node_wrapper
    class(ast_node), allocatable :: node
end type ast_node_wrapper

type(ast_node_wrapper), allocatable :: array(:)
```

### 2. Array Extension Syntax
**Pattern**: `array = [array, new_element]` where `new_element` is a variable (not expression).

**Implementation**:
```fortran
block
    type(ast_node_wrapper) :: new_wrapper
    allocate(new_wrapper%node, source=new_element)
    array = [array, new_wrapper]
end block
```

### 3. Multi-line Construct Parsing
**Challenge**: Fortran constructs span multiple lines (functions, do loops, if blocks).

**Solution**: Parser state machine with proper lookahead:
- Detect program unit boundaries
- Parse complete constructs as single units
- Maintain proper AST hierarchy

## Testing Architecture

### Comprehensive Test Coverage
```
test/frontend/
├── lexer/test_frontend_lexer_*.f90      # Lexer component tests
├── parser/test_frontend_parser_*.f90     # Parser component tests  
├── semantic/test_frontend_semantic_*.f90 # Semantic analysis tests
├── codegen/test_frontend_codegen_*.f90   # Code generation tests
└── test_frontend_integration.f90        # Full pipeline tests
```

### Test Categories
- **Unit tests**: Individual component testing
- **API tests**: Interface validation
- **Integration tests**: Full pipeline validation
- **JSON workflow tests**: Debug output validation

## *Lazy Fortran* Extensions

### Dialect-Specific Features (`src/frontend/standard/lazy_fortran/`)
- **Implicit program wrapping**: No `program`/`end program` needed
- **Automatic type inference**: Variables declared through assignment
- **Modern defaults**: `implicit none`, `real(8)`, `intent(in)`
- **Automatic contains insertion**: For functions/subroutines

### Semantic Analysis Extensions
- **Program vs Module decision**: Deferred to semantic analyzer
- **Type inference enhancement**: Beyond standard Fortran
- **Parameter intent enhancement**: Automatic `intent(in)` addition

## Future Architecture Extensions

### 1. Multi-Standard Support
- **Fortran 2003**: OOP, parameterized types
- **Fortran 2008**: Coarrays, submodules  
- **Fortran 2018**: New features
- **Extensible backends**: Pluggable code generation

### 2. Advanced Type System
- **Multiple dispatch**: Julia-like generic programming
- **Type classes**: Haskell-inspired abstractions
- **Bidirectional checking**: Explicit + inferred types
- **Gradual typing**: Mixed annotation support

### 3. Alternative Backends
- **LLVM IR**: Direct compilation to optimized code
- **C transpilation**: Portable output
- **Native code**: Direct machine code generation
- **Source maps**: Debug information preservation

## Implementation Quality

### Memory Management
- **Safe polymorphic arrays**: Wrapper pattern prevents crashes
- **Proper allocation**: `allocate(var, source=value)` pattern
- **Avoid assignment**: Use allocation for polymorphic components

### Error Handling
- **Graceful degradation**: Fallback to direct output for unimplemented features
- **Clear diagnostics**: Meaningful error messages with source locations
- **Recovery strategies**: Continue parsing after errors when possible

### Performance
- **Efficient parsing**: Single-pass with minimal backtracking
- **Compact AST**: Memory-efficient node representation
- **Fast compilation**: Minimal overhead over traditional approaches

## Success Metrics

### ✅ Completed
- **Complete 4-phase architecture**: All phases implemented and working
- **Hindley-Milner type inference**: Algorithm W with unification
- **Comprehensive testing**: 30+ test files with wildcard discovery
- **JSON workflow**: Debug output for all phases
- **Standard compliance**: Valid Fortran 90 output generation
- **Wrapper pattern**: Safe polymorphic array handling

### 🎯 Current Focus
- **FPM Registry integration**: Official module database
- **Enhanced language features**: Advanced *lazy fortran* syntax
- **Performance optimization**: Faster compilation times
- **Extended type system**: More sophisticated inference

### 🔮 Future Goals
- **LLVM backend**: Direct IR generation
- **Multiple dispatch**: Advanced type system features
- **Full Fortran 2003+**: Modern Fortran feature support
- **Interactive development**: REPL and enhanced tooling

## Architecture Documentation

### Core Principles
1. **Modular design**: Clean interfaces between components
2. **Extensibility**: Easy addition of new language features  
3. **Standard compliance**: Respect Fortran language semantics
4. **Type safety**: Sound type inference with error detection
5. **Performance**: Efficient compilation without sacrificing correctness

This architecture provides a solid foundation for advanced language features while maintaining compatibility with standard Fortran and enabling future extensions like multiple dispatch, advanced type systems, and alternative compilation backends.
