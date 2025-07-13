# Frontend Architecture Refactoring Plan

## Current Architecture Violations

The current `frontend.f90` (950+ lines) violates our existing core component architecture:

### CRITICAL PROBLEMS:
1. **REIMPLEMENTING EXISTING COMPONENTS**
   - Has its own `lex_file()` instead of using `lexer_core.f90`
   - Has its own `parse_tokens()` instead of using `parser_core.f90`  
   - Has its own type inference instead of using `semantic_analyzer_simple.f90`
   - Has its own code generation instead of using `codegen_core.f90`

2. **Direct Token-to-Code Generation**
   - `generate_use_statements_from_tokens()`
   - `generate_executable_statements_from_tokens()`
   - `generate_function_definitions_from_tokens()`
   - `reconstruct_line_from_tokens()`

3. **Bypassing Existing Architecture**
   - Creates placeholder AST then ignores it
   - Stores `current_tokens` for later direct manipulation
   - No proper AST traversal using existing codegen

4. **Ignoring Our Standards Structure**
   - Should use `src/dialects/lazy_fortran/` for extensions
   - Should coordinate existing components, not replace them

## Target Architecture

### USE EXISTING COMPONENTS:
```
Source → lexer_core → parser_core → semantic_analyzer → codegen_core → Output
  (.f)       ↓           ↓              ↓                ↓           (.f90)
          Tokens      AST         Typed AST         Standard Code
```

### ACTUAL Module Structure (USE WHAT EXISTS):
```
src/
├── core/                           # ALREADY EXISTS - USE THESE!
│   ├── lexer_core.f90             # ✅ Core tokenization  
│   ├── parser_core.f90            # ✅ Core parsing
│   ├── ast_core.f90               # ✅ Base AST nodes
│   └── codegen_core.f90           # ✅ Code generation
├── dialects/lazy_fortran/          # ALREADY EXISTS - EXTEND THESE!
│   └── ast_lf.f90                 # ✅ Lazy fortran AST extensions
├── frontend/semantic/              # ALREADY EXISTS - USE THESE!
│   ├── semantic_analyzer.f90      # ✅ Main semantic analysis
│   ├── semantic_analyzer_simple.f90 # ✅ Simplified version  
│   └── type_system_hm.f90         # ✅ Type system
└── frontend/
    └── frontend.f90               # REFACTOR: Coordinate existing components
```

**CRITICAL**: Frontend should be a THIN COORDINATOR calling existing components!

## Refactoring Strategy

### Phase 1: Extract Core Components
- [ ] Move lexer logic to `src/frontend/lexer/`
- [ ] Move parser logic to `src/frontend/parser/` 
- [ ] Move semantic analysis to `src/frontend/semantic/`
- [ ] Move code generation to `src/frontend/codegen/`

### Phase 2: Implement Proper AST Pipeline
- [ ] Remove all `generate_*_from_tokens()` functions
- [ ] Implement AST-based code generation
- [ ] Ensure semantic analysis populates type information
- [ ] Remove `reconstruct_line_from_tokens()` usage

### Phase 3: Clean Separation
- [ ] Separate core Fortran vs lazy fortran dialect code
- [ ] Implement proper fallback for unimplemented features
- [ ] Clean up frontend.f90 to be pure coordinator

### Phase 4: Verification
- [ ] Ensure all tests still pass
- [ ] Verify clean architectural separation
- [ ] Document new architecture

## Implementation Details

### Frontend Coordinator (New)
```fortran
module frontend
  use frontend_lexer
  use frontend_parser  
  use frontend_semantic
  use frontend_codegen
  
  public :: compile_source
  
contains
  subroutine compile_source(input_file, options, error_msg)
    ! Phase 1: Lexical Analysis
    call lex_file(input_file, tokens, error_msg)
    if (error_msg /= "") return
    
    ! Phase 2: Parsing  
    call parse_tokens(tokens, ast, error_msg)
    if (error_msg /= "") return
    
    ! Phase 3: Semantic Analysis
    call analyze_semantics(ast, typed_ast, error_msg) 
    if (error_msg /= "") return
    
    ! Phase 4: Code Generation
    call generate_code(typed_ast, options, code, error_msg)
    if (error_msg /= "") return
    
    ! Write output
    call write_output(code, options%output_file, error_msg)
  end subroutine
end module
```

### AST-Based Code Generation
```fortran
module frontend_codegen_fortran
  use ast_core
  use ast_lazy_fortran
  
contains
  recursive function generate_node(node) result(code)
    class(ast_node), intent(in) :: node
    character(len=:), allocatable :: code
    
    select type (node)
    type is (lf_program_node)
      code = generate_program(node)
    type is (assignment_node)  
      code = generate_assignment(node)
    type is (function_def_node)
      code = generate_function(node)
    ! ... proper AST traversal
    class default
      ! FALLBACK: Direct line printing for unimplemented
      code = "! FALLBACK: " // node%source_line
    end select
  end function
end module
```

### Benefits of Refactoring

1. **Clean Architecture**: Proper 4-phase separation
2. **Maintainability**: Smaller, focused modules
3. **Extensibility**: Easy to add new backends/dialects  
4. **Testability**: Each phase can be tested independently
5. **Performance**: No redundant token processing
6. **Correctness**: Proper semantic analysis drives code generation

## Migration Strategy

### Incremental Approach:
1. **Extract without changing logic** - Move existing code to modules
2. **Implement proper AST pipeline** - Replace token shortcuts  
3. **Add proper semantic analysis** - Type information from HM inference
4. **Clean up and optimize** - Remove redundant code

### Testing Strategy:
- All existing tests must continue to pass
- Add new tests for each extracted module
- Integration tests for full pipeline
- Performance regression tests