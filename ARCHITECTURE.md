# Fortran Compiler Architecture

## Overview

The Fortran compiler implements a 4-phase compilation pipeline:

```
Source Code → Lexer → Parser → Semantic Analysis → Code Generation → Output
```

## Directory Structure

```
src/
├── frontend/
│   ├── frontend.f90              # Main coordinator (290 lines)
│   ├── lexer/
│   │   └── lexer_core.f90        # Tokenization
│   ├── parser/
│   │   └── parser_core.f90       # AST construction
│   ├── semantic/
│   │   ├── semantic_analyzer.f90 # Type inference (Hindley-Milner)
│   │   └── type_system_hm.f90    # Type system implementation
│   ├── codegen/
│   │   └── codegen_core.f90      # Code generation
│   ├── fallback/                 # Temporary token-based fallbacks
│   │   ├── token_fallback.f90
│   │   └── declaration_generator.f90
│   ├── utils/
│   │   ├── debug_utils.f90       # Debug output functions
│   │   └── parser_utils.f90      # Parser helpers
│   └── standard/
│       └── lazy_fortran/         # Lazy Fortran dialect
├── runner/                       # Execution management
├── cache/                        # Compilation caching
└── config/                       # Configuration management
```

## Key Components

### 1. Lexer (lexer_core.f90)
- Tokenizes source code into structured tokens
- Handles keywords, operators, literals, identifiers
- Preserves source location information

### 2. Parser (parser_core.f90)
- Builds Abstract Syntax Tree (AST) from tokens
- Supports expressions, statements, and program structures
- Uses recursive descent parsing

### 3. Type System (type_system_hm.f90)
- Implements Hindley-Milner type inference
- Supports polymorphic types with type variables
- Uses wrapper pattern for polymorphic arrays:
  ```fortran
  type :: mono_type_wrapper
      type(mono_type_t) :: typ
  end type
  ```

### 4. Semantic Analyzer (semantic_analyzer.f90)
- Performs type inference using Algorithm W
- Manages type environments and substitutions
- Handles builtin functions and operations

### 5. Code Generator (codegen_core.f90)
- Traverses AST to generate standard Fortran code
- Adds necessary declarations and structure
- Handles lazy Fortran to standard Fortran translation

## Type System Design

### Type Representation
```fortran
type :: mono_type_t
    integer :: kind                           ! TVAR, TINT, TREAL, etc.
    type(type_var_t) :: var                  ! For type variables
    type(mono_type_wrapper), allocatable :: args(:)  ! For functions/arrays
    integer :: size                          ! For arrays/strings
end type
```

### Type Kinds
- `TVAR` (1): Type variable
- `TINT` (2): Integer type
- `TREAL` (3): Real type
- `TCHAR` (4): Character type
- `TFUN` (5): Function type
- `TARRAY` (6): Array type

## Current Limitations

1. **Function Type Inference**: Crashes when processing function calls
2. **Type Variable Handling**: Issues with substitution application
3. **Limited Type Support**: No user-defined types yet
4. **Fallback Mechanisms**: Still uses token-based fallbacks for some features

## Design Principles

1. **Clean Architecture**: Separation of concerns with focused modules
2. **Type Safety**: Strong type inference with Hindley-Milner
3. **Extensibility**: Support for multiple Fortran standards
4. **Performance**: Efficient caching and compilation
5. **User Experience**: Python-like simplicity for Fortran