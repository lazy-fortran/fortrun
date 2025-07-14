# AST Architecture for *Lazy Fortran* Compiler Frontend

## CRITICAL SITUATION ANALYSIS ⚠️

**CURRENT STATE**: The frontend is fundamentally broken due to **architectural violations** in parsing multi-line constructs.

### Root Cause
The current parser processes tokens **line-by-line** but Fortran constructs are **multi-line** by nature:

```fortran
real function compute(x)  <- Line 1 (function declaration)
  real :: x               <- Line 2 (parameter declaration)  
  compute = x * x         <- Line 3 (function body)
end function              <- Line 4 (function end)
```

**Problem**: Parser calls `parse_statement()` on each line individually, but function definitions span multiple lines. This violates the Fortran 95 standard where functions are **program units**, not statements.

### Evidence from Fortran 95 Standard
According to `doc/standard/Fortran95.md`:
- **Function definitions are program units** (section 7), not statements
- **Program units include**: main program, external procedures, modules, block data
- **Functions have the structure**: `function name(args) ... end function name`

### Current Parser Architecture Violation
```fortran
! Current broken approach:
Line 1: parse_statement("real function compute(x)") → literal_node (WRONG!)
Line 2: parse_statement("real :: x") → assignment_node (WRONG!)
Line 3: parse_statement("compute = x * x") → assignment_node (WRONG!)
Line 4: parse_statement("end function") → unknown_node (WRONG!)
```

**Required**: Multi-line program unit parsing as per Fortran standard.

## ARCHITECTURE REDESIGN PLAN

### Phase 1: Fix Parser Architecture (IMMEDIATE)

**1. Implement Program Unit Parser**
```fortran
module parser_core
  ! Replace line-by-line parsing with program unit parsing
  
  function parse_program_unit(tokens) result(unit)
    type(token_t), intent(in) :: tokens(:)
    class(ast_node), allocatable :: unit
    type(parser_state_t) :: parser
    
    parser = create_parser_state(tokens)
    
    ! Detect program unit type based on first tokens
    if (is_function_definition(parser)) then
      unit = parse_function_definition(parser)
    else if (is_subroutine_definition(parser)) then
      unit = parse_subroutine_definition(parser)
    else if (is_program_definition(parser)) then
      unit = parse_program_definition(parser)
    else
      ! Single statement - this is the current broken case
      unit = parse_statement(parser)
    end if
  end function
  
  function is_function_definition(parser) result(is_func)
    type(parser_state_t), intent(in) :: parser
    logical :: is_func
    
    ! Look for patterns:
    ! 1. "function name(" 
    ! 2. "type function name("
    ! 3. "recursive function name("
    ! 4. "pure function name("
    ! 5. "elemental function name("
    
    is_func = detect_function_pattern(parser%tokens)
  end function
end module
```

**2. Multi-line Statement Collection**
```fortran
subroutine parse_tokens(tokens, ast_tree, error_msg)
  type(token_t), intent(in) :: tokens(:)
  class(ast_node), allocatable, intent(out) :: ast_tree
  character(len=*), intent(out) :: error_msg
  
  ! Group tokens by program units, not lines
  type(token_t), allocatable :: unit_tokens(:)
  class(ast_node), allocatable :: units(:)
  integer :: i, unit_start, unit_end
  
  i = 1
  do while (i <= size(tokens))
    ! Find next program unit boundary
    call find_program_unit_boundary(tokens, i, unit_start, unit_end)
    
    ! Extract tokens for this unit
    unit_tokens = tokens(unit_start:unit_end)
    
    ! Parse the program unit
    units = [units, parse_program_unit(unit_tokens)]
    
    i = unit_end + 1
  end do
  
  ! Create program with all units
  ast_tree = create_program(units)
end subroutine
```

### Phase 2: Implement Proper AST Nodes (URGENT)

**1. Fortran 95 Compliant AST Structure**
```fortran
module ast_core
  ! Base AST node
  type, abstract :: ast_node
    integer :: line, column
    type(mono_type_t), allocatable :: inferred_type
  end type

  ! Program unit nodes (Fortran 95 section 7)
  type, extends(ast_node) :: program_unit_node
    character(len=:), allocatable :: name
  end type
  
  type, extends(program_unit_node) :: function_def_node
    type(ast_node), allocatable :: return_type
    type(ast_node), allocatable :: params(:)
    type(ast_node), allocatable :: body(:)
    logical :: is_recursive = .false.
    logical :: is_pure = .false.
    logical :: is_elemental = .false.
  end type
  
  type, extends(program_unit_node) :: subroutine_def_node
    type(ast_node), allocatable :: params(:)
    type(ast_node), allocatable :: body(:)
    logical :: is_recursive = .false.
    logical :: is_pure = .false.
    logical :: is_elemental = .false.
  end type
  
  type, extends(program_unit_node) :: main_program_node
    type(ast_node), allocatable :: body(:)
    logical :: implicit_program = .false.  ! for lazy fortran
  end type
  
  type, extends(program_unit_node) :: module_node
    type(ast_node), allocatable :: body(:)
    type(ast_node), allocatable :: public_items(:)
    type(ast_node), allocatable :: private_items(:)
  end type
end module
```

**2. Declaration Nodes (Fortran 95 section 4)**
```fortran
module ast_declarations
  use ast_core
  
  type, extends(ast_node) :: declaration_node
    type(ast_node), allocatable :: type_spec
    type(ast_node), allocatable :: variables(:)
    type(ast_node), allocatable :: attributes(:)
  end type
  
  type, extends(ast_node) :: type_spec_node
    integer :: intrinsic_type  ! INTEGER, REAL, COMPLEX, LOGICAL, CHARACTER
    integer :: kind_value
    integer :: char_length
  end type
  
  type, extends(ast_node) :: variable_node
    character(len=:), allocatable :: name
    type(ast_node), allocatable :: initialization
    type(ast_node), allocatable :: dimensions(:)
  end type
  
  type, extends(ast_node) :: intent_node
    integer :: intent_type  ! INTENT_IN, INTENT_OUT, INTENT_INOUT
  end type
end module
```

### Phase 3: Fix Code Generation (URGENT)

**1. Proper Function Definition Generation**
```fortran
function generate_code_function_def(node) result(code)
  type(function_def_node), intent(in) :: node
  character(len=:), allocatable :: code
  
  ! Generate function declaration line
  code = generate_function_declaration(node)
  code = code // new_line('a')
  
  ! Generate function body (declarations + statements)
  code = code // "    implicit none" // new_line('a')
  code = code // generate_parameter_declarations(node%params)
  code = code // generate_return_type_declaration(node)
  code = code // generate_function_body(node%body)
  
  ! Generate function end
  code = code // "end function " // node%name
end function

function generate_function_declaration(node) result(decl)
  type(function_def_node), intent(in) :: node
  character(len=:), allocatable :: decl
  
  ! Handle prefixes
  decl = ""
  if (node%is_recursive) decl = decl // "recursive "
  if (node%is_pure) decl = decl // "pure "
  if (node%is_elemental) decl = decl // "elemental "
  
  ! Add return type if present
  if (allocated(node%return_type)) then
    decl = decl // generate_code_polymorphic(node%return_type) // " "
  end if
  
  ! Add function keyword and name
  decl = decl // "function " // node%name
  
  ! Add parameters
  decl = decl // "(" // generate_parameter_list(node%params) // ")"
end function
```

**2. Remove Bogus Statement Labels**
The `0` appearing in generated code comes from unrecognized AST nodes. Fix by:
```fortran
function generate_code_polymorphic(node) result(code)
  class(ast_node), intent(in) :: node
  character(len=:), allocatable :: code
  
  select type (node)
  type is (assignment_node)
    code = generate_code_assignment(node)
  type is (function_def_node)
    code = generate_code_function_def(node)
  ! ... other cases
  class default
    ! NEVER generate "0" - always generate comment
    code = "! Unimplemented AST node: " // node%node_type_name()
  end select
end function
```

### Phase 4: Re-enable Type Inference (NEXT)

**1. Simplified Type Inference**
```fortran
module semantic_analyzer_simple
  ! Simplified type inference for basic cases
  
  subroutine analyze_program_simple(prog)
    type(main_program_node), intent(inout) :: prog
    
    ! Phase 1: Collect all variable assignments
    call collect_variable_assignments(prog)
    
    ! Phase 2: Infer types from literals
    call infer_from_literals(prog)
    
    ! Phase 3: Propagate types through assignments
    call propagate_assignment_types(prog)
    
    ! Phase 4: Infer function return types
    call infer_function_return_types(prog)
  end subroutine
  
  subroutine infer_from_literals(prog)
    type(main_program_node), intent(inout) :: prog
    
    ! Simple rules:
    ! integer literal → integer type
    ! real literal → real(8) type  
    ! string literal → character type
    ! logical literal → logical type
  end subroutine
end module
```

**2. Function Type Inference**
```fortran
subroutine analyze_function_definition(func)
  type(function_def_node), intent(inout) :: func
  
  ! Step 1: Analyze function body
  call analyze_function_body(func%body)
  
  ! Step 2: Infer return type from return assignments
  call infer_function_return_type(func)
  
  ! Step 3: Enhance parameter types with intent(in)
  call enhance_parameter_types(func%params)
end subroutine
```

## IMPLEMENTATION STRATEGY

### Stage 1: Emergency Fix (IMMEDIATE - Today)
1. **Fix `parse_function_definition`** to handle multi-line constructs
2. **Fix `generate_code_function_def`** to generate proper Fortran
3. **Remove statement label generation** from unrecognized nodes
4. **Test with simple function definitions**

### Stage 2: Architecture Compliance (URGENT - This Week)
1. **Implement program unit parser** following Fortran 95 standard
2. **Replace line-by-line parsing** with proper program unit parsing
3. **Add comprehensive AST nodes** for all Fortran 95 constructs
4. **Test with complex multi-line programs**

### Stage 3: Type System Recovery (NEXT - Next Week)
1. **Re-enable semantic analysis** with simplified type inference
2. **Fix Hindley-Milner implementation** for function types
3. **Add bidirectional type checking** for explicit declarations
4. **Test with type inference scenarios**

### Stage 4: Complete Standard Compliance (FUTURE)
1. **Full Fortran 95 standard compliance** testing
2. **Module system integration**
3. **Advanced type system features**
4. **Alternative backend preparation**

## DESIGN PRINCIPLES

### 1. Fortran Standard Compliance
- Follow Fortran 95 standard structure for program units
- Respect lexical rules and syntax requirements
- Maintain backward compatibility with existing Fortran code

### 2. Modern Parser Architecture
- **Recursive descent parser** with proper lookahead
- **Program unit recognition** before statement parsing
- **Error recovery** with helpful diagnostics
- **Multi-pass analysis** for complex constructs

### 3. Type System Design
- **Hindley-Milner core** for automatic inference
- **Bidirectional checking** for explicit annotations
- **Gradual typing** support for mixed code
- **Multiple dispatch** foundation for future extensions

### 4. Code Generation Quality
- **Readable output** that matches hand-written Fortran
- **Preserve comments** and formatting where possible
- **Efficient compilation** with minimal overhead
- **Multiple backend** support architecture

## SUCCESS METRICS

### Phase 1 Success:
- Basic function definitions parse correctly
- Generated Fortran compiles without errors
- Simple type inference works for variables
- Test suite passes basic functionality

### Phase 2 Success:
- All Fortran 95 program units parse correctly
- Complex multi-line constructs work
- AST accurately represents all language constructs
- Standard Fortran programs pass through unchanged

### Phase 3 Success:
- Complete type inference for all supported constructs
- Function signature enhancement works
- Parameter type inference with intent attributes
- Nested function calls resolve correctly

### Phase 4 Success:
- Full Fortran 95 compatibility
- Ready for alternative backend development
- Foundation for advanced features (multiple dispatch, etc.)
- Production-ready compiler frontend

## COMPILER FRONTEND ARCHITECTURE

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

## EXAMPLE PROCESSING

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

## CRITICAL REQUIREMENTS

1. **No Architecture Violations**: All processing through proper AST pipeline
2. **Standard Compliance**: Follow Fortran 95 standard exactly
3. **Type Safety**: Sound type inference with proper error handling
4. **Performance**: Efficient parsing and code generation
5. **Maintainability**: Clean, modular, testable code structure

This plan addresses the fundamental architectural issues while building toward a complete, standard-compliant compiler frontend that can serve as the foundation for advanced *lazy fortran* features.