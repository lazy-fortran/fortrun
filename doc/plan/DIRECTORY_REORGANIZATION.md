# Directory Structure Reorganization Plan

## Current Confusing Structure

```
src/
├── core/              # Core components  
├── frontend/          # Frontend coordination
│   └── semantic/      # Semantic analysis (confusing location)
├── dialects/          # Standards (confusing name)
├── codegen/           # Separate codegen (duplication?)
├── parser/            # Separate parser (duplication?) 
└── [other modules]
```

**PROBLEMS:**
- Semantic analysis in `frontend/` instead of `core/`
- `dialects/` should be `standards/` 
- Duplicate `codegen/` and `parser/` directories
- Inconsistent organization

## Proposed Clean Structure

```
src/
├── core/                    # Core shared components
│   ├── lexer.f90           # Core tokenization
│   ├── parser.f90          # Core parsing  
│   ├── ast.f90             # Core AST nodes
│   ├── semantic.f90        # Core semantic analysis
│   ├── codegen.f90         # Core code generation
│   └── types.f90           # Type system
├── standards/               # Fortran standard implementations
│   ├── fortran90/          # Fortran 90 standard
│   ├── fortran2018/        # Modern Fortran standard
│   └── lazy_fortran/       # Lazy fortran standard
│       ├── ast_lf.f90      # Lazy fortran AST extensions
│       ├── semantic_lf.f90 # Lazy fortran semantic extensions
│       └── codegen_lf.f90  # Lazy fortran code generation
├── frontend/               # Standard-specific frontends
│   ├── fortran90.f90       # Fortran 90 frontend
│   ├── fortran2018.f90     # Fortran 2018 frontend  
│   └── lazy_fortran.f90    # Lazy fortran frontend
└── [other modules unchanged]
    ├── cli/
    ├── cache/
    ├── config/
    ├── runner/
    ├── notebook/
    └── ...
```

## Migration Steps

### Step 1: Reorganize Core Components
- [ ] Move `src/frontend/semantic/*` → `src/core/semantic.f90`
- [ ] Consolidate `src/core/lexer_core.f90` → `src/core/lexer.f90`
- [ ] Consolidate `src/core/parser_core.f90` → `src/core/parser.f90`
- [ ] Consolidate `src/core/ast_core.f90` → `src/core/ast.f90`
- [ ] Consolidate `src/core/codegen_core.f90` → `src/core/codegen.f90`

### Step 2: Reorganize Standards
- [ ] Rename `src/dialects/` → `src/standards/`
- [ ] Move lazy fortran components to proper structure
- [ ] Remove duplicate directories

### Step 3: Create Clean Frontends
- [ ] Replace monolithic `src/frontend/frontend.f90` with standard-specific frontends
- [ ] Each frontend coordinates core + standard-specific components

### Step 4: Update All References
- [ ] Update `fpm.toml` 
- [ ] Update all imports in source files
- [ ] Update test structure to match
- [ ] Update documentation

## Benefits

1. **Clear Separation**: Core vs standard-specific code
2. **No Duplication**: Single location for each component type
3. **Consistent Naming**: `standards/` not `dialects/`
4. **Logical Organization**: Related files together
5. **Maintainable**: Easy to find and modify components

## Test Structure (to match)

```
test/
├── core/               # Core component tests
│   ├── test_lexer.f90
│   ├── test_parser.f90
│   ├── test_ast.f90
│   ├── test_semantic.f90
│   └── test_codegen.f90
├── standards/          # Standard-specific tests
│   ├── fortran90/
│   ├── fortran2018/
│   └── lazy_fortran/
├── frontend/           # Frontend integration tests
│   ├── test_fortran90_frontend.f90
│   ├── test_fortran2018_frontend.f90
│   └── test_lazy_fortran_frontend.f90
└── [other unchanged]
```

This creates a clean, logical structure where each component has a single responsible location.