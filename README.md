# fortran

**Make Python Fortran again.** - A command-line tool that enables running Fortran programs without manual compilation, automatically resolving dependencies and applying modern defaults.

## Overview

The `fortran` command works like `python` but for `.f90` files. It automatically resolves module dependencies, applies modern defaults, builds with FPM, caches results, and executes programs transparently.

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

## Examples

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
