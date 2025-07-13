# AST Architecture for *Lazy Fortran* Compiler Frontend

## Overview

This document outlines the architecture for a modern compiler frontend for *lazy fortran* with automatic type inference and multiple dispatch. Our experimental dialect pushes beyond all alternative scientific computing languages, exploring how far we can evolve Fortran to surpass Python, Julia, MATLAB, and others in both performance and expressiveness. The frontend provides a complete 4-phase pipeline (Lexer → Parser → Semantic Analysis → Code Generation) that can target multiple backends, with standard Fortran as the initial intermediate representation.

## Design Goals

1. **Simple and Elegant**: Clean separation of compilation phases
2. **High Performance**: Single-pass parsing, efficient memory usage
3. **Extensible**: Easy to add new language features
4. **Multiple Backends**: Support both Fortran generation and direct compiler IR
5. **Type Inference**: Built-in support for automatic type inference
6. **Future-Proof**: Architecture supports planned features like multiple dispatch
7. **Multi-Dialect Support**: Shared functionality for all Fortran standards with specialized modules for dialect-specific features
8. **Superset Design**: *lazy fortran* is a superset of standard Fortran - any valid Fortran 95/2003/2008/2018 program should pass through the frontend unchanged
9. **Beyond Alternatives**: Push beyond all alternative scientific computing languages in expressiveness while maintaining Fortran's performance advantage

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
│   └── lazy_fortran/     # Our simplified dialect
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
              Lazy Fortran    Core+SF   Core+SF          Type Inference
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

#### Lazy Fortran Lexer Extensions

```fortran
module lexer_lazy_fortran
  use lexer_core
  
  ! Additional token types for Lazy Fortran
  integer, parameter :: TK_INDENT = 100     ! Python-like indentation
  integer, parameter :: TK_DEDENT = 101     ! Python-like dedentation
  integer, parameter :: TK_FSTRING = 102    ! f"string {expr}" support
  
  ! Extended tokenization with Lazy Fortran features
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

#### Lazy Fortran AST Extensions

```fortran
module ast_lazy_fortran
  use ast_core
  
  ! Extended program node with implicit program support
  type, extends(program_node) :: sf_program_node
    logical :: implicit = .false.  ! true if auto-wrapped
    logical :: auto_contains = .false.  ! true if contains was auto-inserted
  end type
  
  ! Type-inferred variable (unique to Lazy Fortran)
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

### Phase 3: Semantic Analysis with Hindley-Milner Type Inference

The semantic analyzer uses a Hindley-Milner type inference algorithm, providing an elegant and simple approach with the option to extend to bidirectional type checking later.

#### Core Type System

```fortran
module type_system
  ! Type variables for polymorphic types
  type :: type_var
    integer :: id
    character(len=:), allocatable :: name  ! e.g., 'a, 'b
  end type

  ! Monomorphic types
  type :: mono_type
    integer :: kind  ! TVAR, TINT, TREAL, TCHAR, TFUN, TARRAY
    type(type_var) :: var  ! for TVAR
    type(mono_type), allocatable :: args(:)  ! for TFUN, TARRAY
    integer :: size  ! for TCHAR(len=size)
  end type

  ! Type schemes (polymorphic types)
  type :: poly_type
    type(type_var), allocatable :: forall(:)  ! quantified variables
    type(mono_type) :: mono  ! the monomorphic type
  end type

  ! Type environment
  type :: type_env
    type(hash_table) :: bindings  ! variable -> poly_type
  contains
    procedure :: lookup
    procedure :: extend
    procedure :: generalize
  end type
end module
```

#### Hindley-Milner Algorithm

```fortran
module semantic_analyzer
  use type_system
  
  type :: semantic_context
    type(type_env) :: env
    integer :: next_var_id = 0
    type(substitution) :: subst
  contains
    procedure :: analyze_program
    procedure :: infer  ! Main HM inference
    procedure :: unify  ! Type unification
    procedure :: instantiate  ! Instantiate type scheme
    procedure :: generalize  ! Generalize to type scheme
  end type

  ! Main type inference function (Algorithm W)
  recursive function infer(ctx, env, expr) result(typ)
    type(semantic_context) :: ctx
    type(type_env) :: env
    class(ast_node) :: expr
    type(mono_type) :: typ
    
    select type (expr)
    type is (literal_node)
      ! Literals have known types
      select case (expr%literal_kind)
      case (INTEGER_LITERAL)
        typ = mono_type(kind=TINT)
      case (REAL_LITERAL)
        typ = mono_type(kind=TREAL)
      case (STRING_LITERAL)
        typ = mono_type(kind=TCHAR, size=len(expr%value)-2)
      end select
      
    type is (identifier_node)
      ! Look up identifier in environment
      block
        type(poly_type) :: scheme
        scheme = env%lookup(expr%name)
        typ = ctx%instantiate(scheme)
      end block
      
    type is (assignment_node)
      ! Infer RHS type and bind to LHS
      typ = ctx%infer(env, expr%value)
      call env%extend(expr%target%name, ctx%generalize(env, typ))
      
    type is (binary_op_node)
      ! Infer operand types and unify
      block
        type(mono_type) :: left_typ, right_typ, result_typ
        type(type_var) :: tv
        
        left_typ = ctx%infer(env, expr%left)
        right_typ = ctx%infer(env, expr%right)
        
        ! Create fresh type variable for result
        tv = ctx%fresh_type_var()
        result_typ = mono_type(kind=TVAR, var=tv)
        
        ! Unify based on operator
        select case (expr%operator)
        case ("+", "-", "*", "/")
          call ctx%unify(left_typ, right_typ)
          call ctx%unify(left_typ, result_typ)
        case ("**")
          call ctx%unify(left_typ, right_typ)
          call ctx%unify(left_typ, result_typ)
        end select
        
        typ = ctx%apply_subst(result_typ)
      end block
      
    type is (function_call_node)
      ! Function application
      block
        type(mono_type) :: fun_typ, arg_typ, result_typ
        type(type_var) :: tv
        integer :: i
        
        ! Get function type
        fun_typ = ctx%infer(env, create_identifier(expr%name))
        
        ! Infer argument types
        do i = 1, size(expr%args)
          arg_typ = ctx%infer(env, expr%args(i))
          
          ! Create result type variable
          tv = ctx%fresh_type_var()
          result_typ = mono_type(kind=TVAR, var=tv)
          
          ! Unify function with arg -> result
          call ctx%unify(fun_typ, &
            mono_type(kind=TFUN, args=[arg_typ, result_typ]))
          
          fun_typ = result_typ
        end do
        
        typ = fun_typ
      end block
    end select
  end function infer
  
  ! Type unification
  subroutine unify(ctx, t1, t2)
    type(semantic_context) :: ctx
    type(mono_type) :: t1, t2
    
    ! Apply current substitution
    t1 = ctx%apply_subst(t1)
    t2 = ctx%apply_subst(t2)
    
    if (t1%kind == TVAR) then
      call ctx%bind_var(t1%var, t2)
    else if (t2%kind == TVAR) then
      call ctx%bind_var(t2%var, t1)
    else if (t1%kind == t2%kind) then
      select case (t1%kind)
      case (TINT, TREAL)
        ! Base types unify if equal
      case (TCHAR)
        if (t1%size /= t2%size) then
          error stop "Cannot unify character types of different lengths"
        end if
      case (TFUN, TARRAY)
        ! Recursively unify components
        call ctx%unify_args(t1%args, t2%args)
      end select
    else
      error stop "Type mismatch: cannot unify different types"
    end if
  end subroutine
end module
```

#### Integration with AST Nodes

```fortran
module ast_core
  ! Extended AST nodes to store inferred types
  type, abstract :: ast_node
    integer :: line
    integer :: column
    type(mono_type), allocatable :: inferred_type  ! Added for type info
  contains
    procedure(visit_interface), deferred :: accept
  end type
end module
```

#### Benefits of Hindley-Milner Approach

1. **Simple and Elegant**: Based on well-understood theory
2. **Sound**: Guarantees type safety
3. **Complete**: Infers most general types
4. **Extensible**: Can add constraints for bidirectional checking
5. **No Annotations Required**: Full type inference

#### Future Extension to Bidirectional

The architecture supports future extension to bidirectional type checking:
- Add type annotations to AST nodes
- Implement checking mode alongside inference mode
- Support more advanced features (higher-rank types, etc.)

### Phase 4: Code Generation

Multiple backend targets supported, with standard Fortran as the initial intermediate representation:

```fortran
module codegen
  type, abstract :: code_generator
  contains
    procedure(generate_interface), deferred :: generate
  end type

  ! Backend 1: Generate standard Fortran (our IR for now)
  type, extends(code_generator) :: fortran_generator
    logical :: modern_defaults = .true.
  contains
    procedure :: generate => generate_fortran
  end type

  ! Backend 2: Future LLVM IR generator
  type, extends(code_generator) :: llvm_generator
  contains
    procedure :: generate => generate_llvm_ir
  end type

  ! Backend 3: Future C code generator
  type, extends(code_generator) :: c_generator
  contains
    procedure :: generate => generate_c
  end type

  ! Backend 4: Future direct machine code
  type, extends(code_generator) :: native_generator
    ! Direct compilation to machine code
  contains
    procedure :: generate => generate_native
  end type
end module
```

#### Standard Fortran as Intermediate Representation

Using standard Fortran as our initial IR provides several benefits:
1. **Immediate usability**: Can be compiled by any Fortran compiler
2. **Debugging**: Human-readable intermediate code
3. **Gradual migration**: Can incrementally replace with other backends
4. **Compatibility**: Works with existing Fortran ecosystem

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

### 4. Standard Fortran Compatibility

Since Lazy Fortran is designed as a superset of standard Fortran, the frontend handles all standard Fortran constructs:

```fortran
! Standard Fortran 95 program - passes through unchanged
program test
    implicit none
    real :: x, y
    integer :: i
    
    x = 5.0
    y = sqrt(x)
    
    do i = 1, 10
        print *, i, x**i
    end do
end program test
```

The frontend recognizes this as standard Fortran (explicit program statement, declarations) and generates identical output, ensuring full backward compatibility.

### 5. Future: Multiple Dispatch

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

## Compiler Frontend Architecture

The Lazy Fortran compiler frontend is designed as a modular, extensible system:

```
Source Code (.f, .f90)
        ↓
┌──────────────────────────────────────┐
│        Compiler Frontend             │
│                                      │
│  ┌────────────┐                     │
│  │   Lexer    │ → Tokens            │
│  └────────────┘                     │
│         ↓                           │
│  ┌────────────┐                     │
│  │   Parser   │ → AST               │
│  └────────────┘                     │
│         ↓                           │
│  ┌────────────────────┐             │
│  │ Semantic Analyzer  │ → Typed AST │
│  │ (Hindley-Milner)   │             │
│  └────────────────────┘             │
│         ↓                           │
│  ┌────────────────────┐             │
│  │  Code Generator    │             │
│  └────────────────────┘             │
└──────────────────────────────────────┘
        ↓
   Backend Targets
   ├─ Standard Fortran (current IR)
   ├─ LLVM IR (future)
   ├─ C code (future)
   └─ Native code (future)
```

### Frontend Use Cases

1. **Preprocessor Mode** (current): Frontend + Fortran codegen → compilable .f90
2. **Compiler Mode** (future): Frontend + LLVM backend → executable
3. **Transpiler Mode** (future): Frontend + C backend → portable C code
4. **Analysis Mode**: Frontend only → type checking, optimization analysis

## CRITICAL ARCHITECTURE REQUIREMENT ⚠️

**ABSOLUTELY FORBIDDEN**: Direct token-to-code generation shortcuts
**MANDATORY**: All code generation MUST go through the complete AST pipeline

### Lexer → Parser → AST → Semantic Analysis → Code Generation

Any shortcuts that bypass AST processing violate our fundamental architecture:
- ❌ NO direct token reconstruction
- ❌ NO string manipulation from tokens 
- ❌ NO bypassing semantic analysis
- ✅ ALL processing through proper AST nodes
- ✅ ALL type information from semantic analysis
- ✅ ALL code generation from AST traversal

**For unimplemented features**: Fall back to direct print of input lines, but mark clearly as temporary fallback.

## Implementation Strategy

### Stage 1: Frontend with Fortran Backend (✅ Current)
- Complete 4-phase frontend implementation
- Hindley-Milner type inference
- Standard Fortran as intermediate representation
- Replaces line-based preprocessor

### Stage 2: Enhanced Frontend Features
- Complete Lazy Fortran language support
- Advanced type system features
- Error recovery and diagnostics
- Module system integration

### Stage 3: Alternative Backends
- LLVM IR generation
- Direct optimization on typed AST
- Multiple dispatch implementation
- Native code generation

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