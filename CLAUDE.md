# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Purpose

This project develops a command-line tool called `fortran` that **Makes Python Fortran again**. The tool enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM, with opinionated modern defaults and zero configuration. Our *lazy fortran* dialect pushes beyond all alternative scientific computing languages to combine Fortran's performance with modern expressiveness.

## Test-Driven Development (CRITICAL)

**ALWAYS follow TDD: Write tests FIRST, then implement!**

1. **RED**: Write a failing test first
2. **GREEN**: Write minimal code to pass the test  
3. **REFACTOR**: Clean up code while keeping tests green

## Build System

This project uses FPM (Fortran Package Manager).

### Essential Commands

```bash
# Build and Run
fpm build                         # Build project
fpm run fortran -- example.f90    # Run main app (IMPORTANT: Use -- separator)
fpm run fortran -- --clear-cache  # Clear cache (CRITICAL before testing frontend!)

# Testing
fpm test                          # Run all tests
fpm test test_name                # Run specific test
fpm test > /dev/null              # Run tests quietly (recommended during development)

# Cache Management
fpm run fortran -- --clear-cache  # Clear all cached files
fpm run fortran -- --cache-info   # Show cache statistics

# Debug 4-Phase Compilation Pipeline (JSON output)
fpm run fortran -- example.f90 --debug-tokens    # Phase 1: Tokenization
fpm run fortran -- example.f90 --debug-ast       # Phase 2: AST parsing
fpm run fortran -- example.f90 --debug-semantic  # Phase 3: Type inference (annotated AST)
fpm run fortran -- example.f90 --debug-codegen   # Phase 4: Fortran code generation
```

## Project Architecture

Clean organized directory structure:
- `src/` - Library modules
  - `frontend/` - Complete compilation pipeline (lexer → parser → semantic → codegen)
  - `[utilities]/` - cli/, cache/, config/, runner/, notebook/, etc.
- `app/` - Executable programs
- `test/` - Test programs organized to match src/ structure
  - `frontend/` - Frontend tests with wildcard naming (test_frontend_lexer_*.f90, etc.)
- `example/` - Example programs
  - `basic/` - Simple getting started examples
  - `scientific/` - Scientific computing examples
  - `modules/` - Module usage examples
  - `lazy_fortran/` - Lazy fortran dialect features
  - `frontend_test_cases/` - Frontend test cases with JSON input/output pairs

## CRITICAL: No Shortcuts in Frontend ⚠️

**All code MUST go through**: Lexer → Parser → AST → Semantic Analysis → Code Generation

- ❌ NO direct token-to-code shortcuts
- ✅ ALL processing through proper AST pipeline
- ✅ ALL type inference via semantic analysis

## Current Status

✅ **Production AST Frontend Complete**
- Complete 4-phase compiler architecture with JSON intermediate representations
- Hindley-Milner type inference with Algorithm W
- Comprehensive test suite (30+ frontend tests)

## *Lazy Fortran* Dialect

Experimental dialect features:
1. **Implicit program wrapping** - No need for `program`/`end program`
2. **Automatic type inference** - Variables declared through assignment
3. **Modern defaults** - `implicit none`, `real(8)`, `intent(in)`
4. **Automatic contains insertion** - For functions/subroutines

## Module Registry System

Maps modules to FPM packages via `~/.config/fortran/registry.toml`:
```toml
[packages.fortplotlib]
git = "https://github.com/krystophny/fortplotlib"
prefix = "fortplot"
```

## Testing Strategy

1. **Write tests FIRST** (TDD: red-green-refactor)
2. **Test components individually** - Direct API tests, not full compiler runs
3. **Use JSON for test data** - Each stage accepts/produces JSON
4. **Start with ONE LINE tests** - Minimal test cases first

## ⚠️ CRITICAL FORTRAN PATTERNS ⚠️

### Polymorphic Arrays - Use Wrapper Pattern
```fortran
type :: ast_node_wrapper
    class(ast_node), allocatable :: node
end type ast_node_wrapper
```

### Array Extension - Use Temporary Variables
```fortran
! CORRECT: array = [array, temp_var]
! WRONG: array = [array, function_call()]
```

## Test Categories

```bash
# Frontend components
fpm test test_frontend_lexer_*
fpm test test_frontend_parser_*
fpm test test_frontend_semantic_*
fpm test test_frontend_codegen_*

# Other systems
fpm test test_cli_comprehensive
fpm test test_runner_comprehensive
fpm test test_cache
```

## JSON Test Structure

```
example/frontend_test_cases/single_assignment/
├── input.txt                    # Just "x = 1"
├── expected_tokens.json         # Expected tokenizer output
├── expected_ast.json           # Expected parser output
├── expected_semantic.json      # Expected semantic analysis output
└── expected_code.txt           # Expected generated code
```

## Critical Notes

- **Clear cache before testing frontend**: `fpm run fortran -- --clear-cache`
- **Run tests quietly**: `fpm test > /dev/null`
- **Fortran 95 standard**: https://wg5-fortran.org/N1151-N1200/N1191.pdf
- **CLI options documented in**: doc/index.md


## Reference Documentation

- Fortran standards: doc/standard/
- Current tasks: TODO.md
- Architecture: doc/design/
- Roadmap: ROADMAP.md
