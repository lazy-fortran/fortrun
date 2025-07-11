![fortran](media/logo.png)

[![codecov](https://codecov.io/gh/krystophny/fortran/branch/main/graph/badge.svg)](https://codecov.io/gh/krystophny/fortran)
[![Documentation](https://img.shields.io/badge/docs-FORD-blue.svg)](https://krystophny.github.io/fortran/)

**Make Python Fortran again.** - A command-line tool that enables running Fortran programs without manual compilation, automatically resolving dependencies and applying modern defaults.

## Overview

The `fortran` command works like `python` but for Fortran files - supporting both modern `.f90` and simplified `.f` formats. It automatically resolves module dependencies, applies modern defaults, builds with FPM, caches results, and executes programs transparently.

The `.f` format brings the simplicity of classic FORTRAN with a modern twist - no boilerplate, automatic type inference, and implicit none by default.

## Quick Start

```bash
# Run any Fortran program instantly
fortran hello.f90

# Simplified .f files (no boilerplate needed!)
fortran script.f  # Just write code, no program/contains needed

# Works with local modules automatically
fortran calculator.f90  # Uses math_module.f90 in same directory

# Verbose output for debugging
fortran -v myprogram.f90
```

## Features

**Zero Configuration**
- No Makefiles, no build scripts, no project setup
- Just write Fortran code and run it
- Automatic dependency detection and resolution

**Opinionated Modern Defaults**
- `implicit none` enforced automatically
- Double precision (`real(8)`) as default for `real` variables
- Free form source format
- Modern compiler flags applied automatically

**Smart Dependency Resolution**
- Local modules: Automatically includes `.f90` files from same directory
- Package registry: Resolves external modules to git repositories
- Interdependent modules: Handles complex dependency chains
- Caching: Builds cached for fast subsequent runs

**Simplified Syntax (.f files)**
- No boilerplate: Skip `program`/`end program` statements
- Auto-wrapping: Functions and subroutines work without `contains`
- Type inference: Automatic variable declarations from assignments
- Script-like: Write Fortran like Python scripts

## Example: Same Program, Two Styles

Compare the same calculation in both formats:
- **Modern Fortran (.f90)**: [example/type_inference/calculate.f90](example/type_inference/calculate.f90)
- **Simplified Format (.f)**: [example/type_inference/calculate.f](example/type_inference/calculate.f)

The `.f` format automatically infers types from assignments and expressions, eliminating all declaration boilerplate while producing identical results.

For a comprehensive demonstration of type inference across all basic types (integer, real, character, logical), see [example/type_inference/all_types.f](example/type_inference/all_types.f).

## More Examples

See `example/` directory for working examples including:
- Basic hello world (`example/hello/`)
- Local module usage (`example/calculator/`)
- Complex interdependent modules (`example/interdependent/`)
- Simplified syntax for `.f` files (`example/simple/`)

## Installation

**Prerequisites:** Modern Fortran compiler (gfortran 9+) and FPM (Fortran Package Manager)

```bash
git clone https://github.com/krystophny/fortran.git
cd fortran
./install.sh
```

## Configuration

The tool uses standard OS directories:
- Config: `~/.config/fortran/` (Linux/macOS)
- Cache: `~/.cache/fortran/` (Linux/macOS)
- Registry: `~/.config/fortran/registry.toml`

The registry maps module names to git repositories. See `registry.toml` for examples.

## Development Status

Current implementation includes CLI, local module resolution, FPM integration, caching, package registry, and type inference for `.f` files. For detailed roadmap and development status, see `ROADMAP.md` and `TODO.md`.

## Contributing

This project follows TDD (Test-Driven Development):
- Write tests first
- Keep changes small and focused
- Comprehensive test coverage for all features
- Clear documentation and examples

## License

MIT License - see LICENSE file for details.

---

*"Fortran is the foundation of scientific computing - now with the developer experience it deserves."*
