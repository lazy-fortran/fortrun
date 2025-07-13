# AST Architecture for Simple Fortran Compiler Frontend

## Overview

This document outlines the architecture for a modern lexer/parser/AST system to replace the current line-based preprocessor, supporting the long-term goal of creating a new compiler frontend for Simple Fortran with automatic type inference and multiple dispatch.

## Design Goals

1. **Simple and Elegant**: Clean separation of compilation phases
2. **High Performance**: Single-pass parsing, efficient memory usage
3. **Extensible**: Easy to add new language features
4. **Multiple Backends**: Support both Fortran generation and direct compiler IR
5. **Type Inference**: Built-in support for automatic type inference
6. **Future-Proof**: Architecture supports planned features like multiple dispatch
7. **Multi-Dialect Support**: Shared functionality for all Fortran standards with specialized modules for dialect-specific features

## Architecture: Shared Core with Dialect Specialization

### Module Organization

```
src/
├── core/                    # Shared functionality for all Fortran dialects
│   ├── lexer_core.f90      # Base tokenization for standard Fortran
│   ├── parser_core.f90     # Core parsing for common constructs
│   ├── ast_core.f90        # Base AST nodes shared by all dialects
│   └── codegen_core.f90    # Common code generation utilities
├── dialects/               # Dialect-specific extensions
│   ├── fortran90/          # Fortran 90 standard support
│   │   ├── lexer_f90.f90   # F90-specific tokens
│   │   └── parser_f90.f90  # F90-specific parsing
│   ├── fortran2018/        # Modern Fortran support
│   │   └── parser_f2018.f90
│   └── simple_fortran/     # Our simplified dialect
│       ├── lexer_sf.f90    # Additional tokens (e.g., Python-like)
│       ├── parser_sf.f90   # Implicit program wrapping, etc.
│       └── inference.f90   # Type inference engine
├── lexer.f90               # Main lexer interface
├── parser.f90              # Main parser interface
└── ast.f90                 # Complete AST definitions
```

### Compilation Pipeline

```
Source (.f) → Dialect Detection → Lexer → Parser → AST → Semantic Analysis → Code Generation
                     ↓               ↓        ↓                    ↓
              Simple Fortran    Core+SF   Core+SF          Type Inference
                                Tokens    Rules            (SF specific)
```

### Phase 1: Lexer (Tokenizer)

#### Core Lexer (Shared)

```fortran
module lexer_core
  ! Base token type used by all dialects
  type :: token
    integer :: kind
    character(len=:), allocatable :: text
    integer :: line
    integer :: column
  end type token

  ! Standard Fortran tokens (shared by all dialects)
  integer, parameter :: TK_IDENTIFIER = 1
  integer, parameter :: TK_NUMBER = 2
  integer, parameter :: TK_STRING = 3
  integer, parameter :: TK_OPERATOR = 4
  integer, parameter :: TK_KEYWORD = 5
  integer, parameter :: TK_NEWLINE = 6
  integer, parameter :: TK_EOF = 7
  
  ! Core tokenization for standard Fortran
  interface
    subroutine tokenize_core(source, tokens)
      character(len=*), intent(in) :: source
      type(token), allocatable, intent(out) :: tokens(:)
    end subroutine
  end interface
end module
```

#### Simple Fortran Lexer Extensions

```fortran
module lexer_simple_fortran
  use lexer_core
  
  ! Additional token types for Simple Fortran
  integer, parameter :: TK_INDENT = 100     ! Python-like indentation
  integer, parameter :: TK_DEDENT = 101     ! Python-like dedentation
  integer, parameter :: TK_FSTRING = 102    ! f"string {expr}" support
  
  ! Extended tokenization with Simple Fortran features
  interface
    subroutine tokenize_sf(source, tokens, dialect_options)
      character(len=*), intent(in) :: source
      type(token), allocatable, intent(out) :: tokens(:)
      type(sf_options), intent(in) :: dialect_options
    end subroutine
  end interface
end module
```

### Phase 2: AST Definition

#### Core AST Nodes (Shared)

```fortran
module ast_core
  ! Base AST node used by all dialects
  type, abstract :: ast_node
    integer :: line
    integer :: column
  contains
    procedure(visit_interface), deferred :: accept
  end type

  ! Standard Fortran nodes (shared by all dialects)
  type, extends(ast_node) :: program_node
    character(len=:), allocatable :: name
    type(ast_node), allocatable :: body(:)
  end type

  type, extends(ast_node) :: assignment_node
    type(ast_node), allocatable :: target
    type(ast_node), allocatable :: value
  end type

  type, extends(ast_node) :: binary_op_node
    type(ast_node), allocatable :: left
    type(ast_node), allocatable :: right
    character(len=:), allocatable :: operator
  end type

  type, extends(ast_node) :: function_def_node
    character(len=:), allocatable :: name
    type(ast_node), allocatable :: params(:)
    type(ast_node), allocatable :: return_type
    type(ast_node), allocatable :: body(:)
  end type

  type, extends(ast_node) :: identifier_node
    character(len=:), allocatable :: name
  end type

  type, extends(ast_node) :: literal_node
    character(len=:), allocatable :: value
    integer :: literal_kind  ! INTEGER_LITERAL, REAL_LITERAL, etc.
  end type
end module
```

#### Simple Fortran AST Extensions

```fortran
module ast_simple_fortran
  use ast_core
  
  ! Extended program node with implicit program support
  type, extends(program_node) :: sf_program_node
    logical :: implicit = .false.  ! true if auto-wrapped
    logical :: auto_contains = .false.  ! true if contains was auto-inserted
  end type
  
  ! Type-inferred variable (unique to Simple Fortran)
  type, extends(ast_node) :: inferred_var_node
    character(len=:), allocatable :: name
    type(ast_node), allocatable :: initial_value
    ! Type will be inferred during semantic analysis
  end type
  
  ! List comprehension node (future Python-like feature)
  type, extends(ast_node) :: list_comp_node
    type(ast_node), allocatable :: expr
    type(ast_node), allocatable :: target
    type(ast_node), allocatable :: iter
    type(ast_node), allocatable :: condition  ! optional
  end type
end module
```

### Phase 3: Semantic Analysis with Type Inference

```fortran
module semantic_analyzer
  type :: semantic_context
    type(symbol_table) :: symbols
    type(type_environment) :: types
    type(function_registry) :: functions
  contains
    procedure :: analyze_program
    procedure :: infer_expression_type
    procedure :: resolve_function_call
  end type

  ! Type inference for assignments
  subroutine analyze_assignment(ctx, node)
    type(semantic_context) :: ctx
    type(assign_node) :: node
    type(type_info) :: expr_type
    
    ! Infer type from RHS
    expr_type = ctx%infer_expression_type(node%value)
    
    ! Add variable with inferred type
    call ctx%symbols%add_variable(node%target%name, expr_type)
  end subroutine
end module
```

### Phase 4: Code Generation

Two backends supported:

```fortran
module codegen
  type, abstract :: code_generator
  contains
    procedure(generate_interface), deferred :: generate
  end type

  ! Generate standard Fortran
  type, extends(code_generator) :: fortran_generator
    logical :: modern_defaults = .true.
  contains
    procedure :: generate => generate_fortran
  end type

  ! Future: Generate compiler IR
  type, extends(code_generator) :: ir_generator
    ! Interface to LLVM, GCC, or custom IR
  contains
    procedure :: generate => generate_ir
  end type
end module
```

## Key Features Support

### 1. Automatic Type Inference

```fortran
! Input:
x = 5.0
y = x * 2

! AST representation enables:
! - Track that 5.0 is real(8) literal
! - Propagate type to x
! - Infer y as real(8) from expression
```

### 2. Implicit Program Wrapping

```fortran
! Parser automatically wraps non-program code:
if (.not. has_program_stmt) then
  root = program_node(name="main", body=statements, implicit=.true.)
end if
```

### 3. Modern Defaults

```fortran
! During semantic analysis:
! - real → real(8)
! - Function parameters → intent(in)
! - Automatic implicit none
```

### 4. Future: Multiple Dispatch

```fortran
! AST structure supports function overloading:
type :: function_signature
  character(len=:), allocatable :: name
  type(type_info), allocatable :: param_types(:)
  type(type_info) :: return_type
end type

! Registry for multiple implementations
type :: dispatch_table
  type(function_signature), allocatable :: signatures(:)
  type(function_node), allocatable :: implementations(:)
end type
```

## Implementation Strategy

### Stage 1: Minimal Lexer/Parser (Current Goal)
- Focus on subset needed for existing preprocessor features
- Generate Fortran code (replace current line-based approach)
- Reuse existing type inference module

### Stage 2: Full Parser
- Complete Fortran subset support
- Better error recovery and reporting
- Symbol table with proper scoping

### Stage 3: Advanced Features
- Multiple dispatch support
- Direct compiler IR generation
- Optimization passes on AST

## Benefits Over Current Approach

1. **Single Pass**: Parse once, analyze and transform on AST
2. **Better Errors**: Precise source locations and context
3. **Extensibility**: New features added as AST nodes
4. **Performance**: No string manipulation, efficient tree traversal
5. **Maintainability**: Clean phase separation, modular design
6. **Future-Proof**: Supports planned advanced features

## Example Processing

Input:
```fortran
x = 5.0
y = sqrt(x**2 + 3.0**2)
```

AST:
```
Program(implicit=true)
 ├─ Assign
 │   ├─ Var("x")
 │   └─ Literal(5.0, type=real8)
 └─ Assign
     ├─ Var("y")
     └─ Call("sqrt")
         └─ BinOp("+")
             ├─ BinOp("**")
             │   ├─ Var("x")
             │   └─ Literal(2, type=int)
             └─ BinOp("**")
                 ├─ Literal(3.0, type=real8)
                 └─ Literal(2, type=int)
```

This AST enables straightforward type inference, optimization, and code generation for both Fortran output and future compiler IR targets.