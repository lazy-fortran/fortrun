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

## Compilation Pipeline

```
Source (.f) → Lexer → Parser → AST → Semantic Analysis → Code Generation
                                 ↓           ↓
                          Type Inference  Symbol Table
                                 ↓
                          Type Environment
```

### Phase 1: Lexer (Tokenizer)

Converts character stream into tokens:

```fortran
module simple_fortran_lexer
  type :: token
    integer :: kind
    character(len=:), allocatable :: text
    integer :: line
    integer :: column
  end type token

  ! Token kinds
  integer, parameter :: TK_IDENTIFIER = 1
  integer, parameter :: TK_NUMBER = 2
  integer, parameter :: TK_STRING = 3
  integer, parameter :: TK_OPERATOR = 4
  integer, parameter :: TK_KEYWORD = 5
  integer, parameter :: TK_NEWLINE = 6
  integer, parameter :: TK_EOF = 7
  ! ... more token types
end module
```

### Phase 2: Parser

Builds Abstract Syntax Tree from tokens:

```fortran
module simple_fortran_ast
  ! Base AST node
  type, abstract :: ast_node
    integer :: line
    integer :: column
  contains
    procedure(visit_interface), deferred :: accept
  end type

  ! Program node (implicit or explicit)
  type, extends(ast_node) :: program_node
    character(len=:), allocatable :: name
    type(ast_node), allocatable :: body(:)
    logical :: implicit  ! true if auto-wrapped
  end type

  ! Assignment node
  type, extends(ast_node) :: assign_node
    type(ast_node), allocatable :: target
    type(ast_node), allocatable :: value
  end type

  ! Binary operation node
  type, extends(ast_node) :: binop_node
    type(ast_node), allocatable :: left
    type(ast_node), allocatable :: right
    character(len=:), allocatable :: op
  end type

  ! Function definition
  type, extends(ast_node) :: function_node
    character(len=:), allocatable :: name
    type(param_node), allocatable :: params(:)
    type(type_spec), allocatable :: return_type  ! optional
    type(ast_node), allocatable :: body(:)
    logical :: auto_contains  ! true if contains was auto-inserted
  end type

  ! Variable reference
  type, extends(ast_node) :: var_node
    character(len=:), allocatable :: name
  end type

  ! Literal values
  type, extends(ast_node) :: literal_node
    type(type_info) :: inferred_type
    character(len=:), allocatable :: value
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