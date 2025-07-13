# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Purpose

This project develops a command-line tool called `fortran` that **Makes Python Fortran again**. The tool enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM, with opinionated modern defaults and zero configuration.

## Build System

This project uses FPM (Fortran Package Manager).

### Essential Commands

```bash
fpm build                         # Build project
fpm run fortran -- example.f90    # Run main app (IMPORTANT: Use -- separator)
fpm test                          # Run all tests
fpm test test_name                # Run specific test
rm -rf ~/.cache/fortran/*         # Clear cache (CRITICAL before testing preprocessor!)
```

## Project Architecture

Standard FPM directory structure:
- `src/` - Library modules
  - `core/` - Shared functionality (lexer, parser, AST, codegen)
  - `dialects/` - Dialect-specific extensions
    - `simple_fortran/` - Our simplified Fortran dialect
- `app/` - Executable programs
- `test/` - Test programs with comprehensive coverage
  - `test_data/` - Test input files
- `example/` - Example programs

### Key Patterns
- **Explicit Typing**: `implicit none` everywhere
- **Private-by-Default**: Modules use private visibility by default
- **Test-Driven Development**: Write tests first

## Development Configuration

The `fpm.toml` enforces: no implicit typing, free-form source, JSON support via json-fortran.

## Current Status

### 🚧 **Phase 8 In Progress**: Advanced Type Inference
- Array type inference
- Derived type support
- Function return type inference
- More sophisticated expression analysis

## Simple Fortran Dialect

1. **Implicit program wrapping** - No need for `program`/`end program`
2. **Automatic type inference** - Variables declared through assignment
3. **Modern defaults** - `implicit none`, `real(8)`, `intent(in)`
4. **Automatic contains insertion** - For functions/subroutines
5. **Future features** - List comprehensions, f-strings, enhanced arrays

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
- **Clear cache before testing preprocessor**: `rm -rf ~/.cache/fortran/*`
- **AST Preprocessor is default** for .f files (use FORTRAN_USE_AST_PREPROCESSOR=0 for legacy)
- Debug apps go in `app/`, then move to `test/` when ready
- Test data goes in `test/test_data/`
- Polymorphic arrays: use `allocate(array, source=input)`
- Avoid polymorphic assignment with allocatable components
- Reference: Fortran 95 standard at https://wg5-fortran.org/N1151-N1200/N1191.pdf