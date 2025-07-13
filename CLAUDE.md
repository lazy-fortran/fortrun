# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Purpose

This project develops a command-line tool called `fortran` that **Makes Python Fortran again**. The tool enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM, with opinionated modern defaults and zero configuration. Our *lazy fortran* dialect pushes beyond all alternative scientific computing languages to combine Fortran's performance with modern expressiveness.

## Build System

This project uses FPM (Fortran Package Manager).

### Essential Commands

```bash
fpm build                         # Build project
fpm run fortran -- example.f90    # Run main app (IMPORTANT: Use -- separator)
fpm test                          # Run all tests
fpm test test_name                # Run specific test
rm -rf ~/.cache/fortran/*         # Clear cache (CRITICAL before testing frontend!)

# Debug Pipeline Stages (JSON output)
fpm run fortran -- example.f90 --debug-tokens    # Debug tokenization stage
fpm run fortran -- example.f90 --debug-ast       # Debug AST parsing stage  
fpm run fortran -- example.f90 --debug-codegen   # Debug code generation stage
fpm run fortran -- example.f90 --debug-tokens --debug-ast --debug-codegen  # Debug all stages
```

## Project Architecture

Standard FPM directory structure:
- `src/` - Library modules
  - `core/` - Shared functionality (lexer, parser, AST, codegen)
  - `dialects/` - Dialect-specific extensions
    - `lazy_fortran/` - Our *lazy fortran* dialect
- `app/` - Executable programs
- `test/` - Test programs with comprehensive coverage
- `example/` - Example programs
  - `test/` - Test input files and expected outputs

### Key Patterns
- **Explicit Typing**: `implicit none` everywhere
- **Private-by-Default**: Modules use private visibility by default
- **Test-Driven Development**: Write tests first

## CRITICAL ARCHITECTURE REQUIREMENT ⚠️

**ABSOLUTELY FORBIDDEN**: Direct token-to-code generation shortcuts in frontend
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

## Development Configuration

The `fpm.toml` enforces: no implicit typing, free-form source, JSON support via json-fortran.

## Current Status

### 🚧 **Phase 8 In Progress**: Advanced Type Inference
- Array type inference
- Derived type support
- Function return type inference
- More sophisticated expression analysis

## *Lazy Fortran* Dialect

**Experimental dialect** - *lazy fortran* is our experimental dialect that pushes the boundaries beyond all alternative languages for scientific computing. It explores how far we can evolve Fortran to surpass Python, Julia, MATLAB, and others in both performance and expressiveness while maintaining full backward compatibility.

1. **Implicit program wrapping** - No need for `program`/`end program`
2. **Automatic type inference** - Variables declared through assignment
3. **Modern defaults** - `implicit none`, `real(8)`, `intent(in)`
4. **Automatic contains insertion** - For functions/subroutines
5. **Future experimental features** - List comprehensions, f-strings, enhanced arrays, pattern matching

### Compiler Flags
```bash
--flag "-fdefault-real-8 -fdefault-double-8"
```

## Module Registry System

Maps Fortran modules to packages, FPM Registry compatible.

```toml
# ~/.config/fortran/registry.toml
[packages.fortplotlib]
git = "https://github.com/krystophny/fortplotlib"
prefix = "fortplot"  # Modules starting with "fortplot"
```

### Resolution Strategy
1. Explicit mappings
2. Custom prefixes
3. Automatic inference (`module_name` → `package-name`)

### Directories
- **Config**: `~/.config/fortran/`
- **Cache**: `~/.cache/fortran/`
- **Registry**: `~/.config/fortran/registry.toml`

## Development Principles

- **Opinionated for Good** - Enforce modern Fortran practices
- **Python-like Experience** - Just run without compilation worries
- **Test-Driven Development** - Tests before code
- **Zero Configuration** - Works out of the box

## Future Roadmap

- Phase 9: Advanced AST Features
- Phase 10: Full AST Integration
- Phase 11: Advanced type inference
- Phase 12: Python-like features
- Phase 13: Enhanced caching
- Phase 14: Official FPM registry integration
- Phase 15: Interactive REPL

**Goal**: Make Fortran development as seamless as Python.

## Critical Development Notes

- **ALWAYS write tests first!** (TDD: red-green-refactor)
- **Clear cache before testing frontend**: `rm -rf ~/.cache/fortran/*`
- **Compiler frontend is used** for .f files (*lazy fortran* with type inference)
- Debug apps go in `app/`, then move to `test/` when ready
- Test data goes in `example/test/`
- **IMPORTANT: When debugging parser/frontend issues, ALWAYS create test cases in example/test/ with:**
  - One subdirectory per test case (e.g., `example/test/use_statement/`)
  - Input file: `<case_name>.f` (e.g., `use_statement.f`)
  - Expected output: `<case_name>.f90` (e.g., `use_statement.f90`)
  - Intermediate representations: `<case_name>_tokens.json`, `<case_name>_ast.json`
  - Add these to automated test coverage immediately via `test_example_test_cases.f90`
- Polymorphic arrays: use `allocate(array, source=input)`
- Avoid polymorphic assignment with allocatable components
- Reference: Fortran 95 standard at https://wg5-fortran.org/N1151-N1200/N1191.pdf
- **To run the currently developed version of fortran, run fpm run fortran -- <arguments>. Always clear cache if you change frontend features.**
- **For debugging AST pipeline issues, use --debug-tokens, --debug-ast, --debug-codegen flags for JSON intermediate output**
- In order not to fill your context uselessly, run full test suite only with "fpm test > /dev/null" to suppress verbose output. Prefer using tests for one subsystem only for development.
- **Command line options are documented in doc/index.md** - this includes all flags, debug options, and usage examples

## Test Categories for Targeted Testing

Run specific test categories during development to avoid context overload:

### Core Language Features
```bash
fpm test test_lexer_basic          # Tokenization
fpm test test_parser_basic         # AST parsing  
fpm test test_codegen_basic        # Code generation
fpm test test_frontend             # Compiler frontend
```

### Type System
```bash
fpm test test_type_inference       # Type inference engine
fpm test test_derived_type_analyzer # Derived types
fpm test test_function_analyzer    # Function analysis
```

### CLI and Runner
```bash
fpm test test_cli_comprehensive    # CLI argument parsing (unit)
fpm test test_cli_system          # CLI system integration 
fpm test test_runner_comprehensive # Full execution pipeline
```

### Caching System
```bash
fpm test test_cache               # Core caching
fpm test test_module_cache_unit   # Module cache unit tests
fpm test test_fpm_cache_integration # FPM integration
```

### Notebook System
```bash
fpm test test_notebook_parser     # Notebook parsing
fpm test test_notebook_executor   # Notebook execution
fpm test test_figure_capture      # Figure capture
```

### Registry and Config
```bash
fpm test test_registry_resolver   # Module resolution
fpm test test_config_extended     # Configuration system
```

### Integration and Examples
```bash
fpm test test_examples            # Example programs
fpm test test_parse_and_codegen   # Full AST pipeline
```

## Reference Documentation

- You can find Fortran and fortran standards for implementation reference in doc/standard
- You can find current status and tasks in TODO.md
- You can find architecture and design plans in doc/plan directory. Our plans live there and we derive TODO.md for concrete implementation planning
- You can find roadmap in ROADMAP.md