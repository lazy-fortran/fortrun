# Frontend Architecture Refactoring Plan

## Current Architecture Violations

The current `frontend.f90` (950+ lines) violates our core 4-phase architecture:

### Problems:
1. **Direct Token-to-Code Generation**
   - `generate_use_statements_from_tokens()`
   - `generate_executable_statements_from_tokens()`
   - `generate_function_definitions_from_tokens()`
   - `reconstruct_line_from_tokens()`

2. **Bypassing AST Pipeline**
   - `parse_tokens()` creates placeholder AST then ignores it
   - Stores `current_tokens` for later direct manipulation
   - No proper AST traversal for code generation

3. **Semantic Analysis Bypass**
   - Type inference during code generation instead of semantic phase
   - No proper type system integration
   - Direct string-based type inference

4. **Monolithic Design**
   - 950+ lines in single file
   - Mixed responsibilities
   - No clean phase separation

## Target Architecture

### Clean 4-Phase Pipeline:
```
Source → Lexer → Parser → Semantic Analysis → Code Generation
  (.f)     ↓       ↓            ↓                ↓
        Tokens   AST      Typed AST        Output (.f90)
```

### Module Structure:
```
src/frontend/
├── frontend.f90              # Main coordinator (< 100 lines)
├── lexer/
│   ├── lexer_interface.f90   # Common lexer interface
│   └── lexer_lazy_fortran.f90 # Lazy fortran extensions
├── parser/
│   ├── parser_interface.f90  # Common parser interface  
│   └── parser_lazy_fortran.f90 # Lazy fortran extensions
├── semantic/
│   ├── semantic_analyzer.f90 # Main semantic analysis
│   ├── type_inference.f90    # Hindley-Milner implementation
│   └── symbol_table.f90      # Symbol management
└── codegen/
    ├── codegen_interface.f90 # Backend interface
    ├── codegen_fortran.f90   # Standard Fortran backend
    └── codegen_fallback.f90  # Direct line printing fallback
```

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