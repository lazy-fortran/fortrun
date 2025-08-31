![fortrun](media/logo.svg)

[![codecov](https://codecov.io/gh/lazy-fortran/fortrun/branch/main/graph/badge.svg)](https://codecov.io/gh/lazy-fortran/fortrun)
[![Documentation](https://img.shields.io/badge/docs-FORD-blue.svg)](https://lazy-fortran.github.io/fortrun/)

A Fortran runner with automatic dependency resolution, intelligent caching, and frontend analysis support.

Our experimental *lazy fortran* dialect pushes beyond all alternative scientific computing languages to combine Fortran's performance with modern expressiveness!

## Quick Start

```bash
# Run any Fortran program instantly
fortrun hello.f90

# *lowercase fortran* .lf files (no boilerplate needed!)
fortrun script.lf

# Notebook mode - now available via fortbook package
# Install: https://github.com/lazy-fortran/fortbook

# Verbose mode for debugging
fortrun -v myprogram.f90

# Cache management
fortrun --clear-cache               # Clear all cached files
fortrun --clear-cache example.f90   # Clear cache and run
fortrun --cache-info                # Show cache statistics

# Compilation - now available via ffc package
# Install: https://github.com/lazy-fortran/ffc
```

## *lowercase fortran* Showcase

Write fortran code with **zero boilerplate** - just the logic you need:

```fortran
x = 5.0
y = 3.0
z = sqrt(x**2 + y**2)
print *, "Distance:", z

result = distance(3.0, 4.0)
print *, "Function result:", result

real function distance(a, b)
  real :: a, b
  distance = sqrt(a**2 + b**2)
end function distance
```

**Runs as:** `fortrun calculate.lf`

**Automatically transforms to:**
- ✅ Wrapped in `program` statement
- ✅ Modern implicit typing via automatic type inference
- ✅ Double precision defaults (`real(8)`)
- ✅ Type declarations automatically generated
- ✅ `contains` section for functions
- ✅ Function parameters default to `intent(in)` for safety

## Features

🚀 **Zero Configuration**
- No Makefiles, build scripts, or project setup
- Automatic dependency detection and resolution
- Smart caching with 2-4x performance improvements

🎯 **Two Execution Modes**  
- **Standard Fortran** (`.f90` files): Modern Fortran with no opinionated changes
- ***lowercase fortran*** (`.lf` files): Compiler frontend with automatic type inference, no boilerplate, `real(8)` defaults

📦 **Smart Dependencies**
- Local modules: Auto-includes Fortran files from same directory
- Package registry: Resolves external modules to git repositories  
- FPM integration: Leverages existing Fortran ecosystem

🚀 **Advanced Features**
- **Type Inference**: Automatic variable declarations in `.lf` files (via [fortfront](https://github.com/lazy-fortran/fortfront))
- **Incremental Compilation**: Only rebuilds changed files
- **Cache Management**: CLI commands to clear cache and view statistics

📦 **Extended Ecosystem**
- **[fortfront](https://github.com/lazy-fortran/fortfront)**: Core analysis frontend (lexer, parser, semantic analysis)
- **[ffc](https://github.com/lazy-fortran/ffc)**: Fortran Fortran Compiler - MLIR backend for compilation via HLFIR/LLVM
- **[fnb](https://github.com/lazy-fortran/fnb)**: Fortran notebook interface - jupytext-style interactive computing with markdown/PDF output

## Examples

| Feature | Example | Link |
|---------|---------|------|
| **Hello World** | Simple program | [hello.f90](example/hello/) |
| **Local Modules** | Calculator with math module | [calculator.f90](example/calculator/) |
| ***lowercase fortran* Syntax** | Type inference showcase | [all_types.lf](example/type_inference/) |
| **Interdependent Modules** | Complex dependency chain | [main.f90](example/interdependent/) |
| **Notebook Mode** | Interactive analysis | [simple_math.lf](example/notebook/) |
| **Plotting (WIP)** | Figure generation | [plotting_demo.lf](example/notebook/) |

## Installation

```bash
git clone https://github.com/krystophny/fortran
cd fortran
./install.sh
```

## Documentation

- 📋 **[ROADMAP.md](ROADMAP.md)** - Development phases and future plans
- 📝 **[TODO.md](TODO.md)** - Current development tasks and progress
- 🏗️ **[CLAUDE.md](CLAUDE.md)** - Technical implementation details

## Usage

```bash
# Two execution modes
fortrun program.f90           # Standard Fortran mode
fortrun script.lf              # lowercase fortran mode (compiler frontend)

# Notebook mode - moved to fnb package
# fnb analysis.lf # Jupytext-style interactive analysis

# Options
fortrun -v file.f90           # Verbose output
fortrun --cache-dir DIR file  # Custom cache directory
fortrun --flag "-O3" file.f90 # Pass custom flags to FPM compiler

# Help
fortrun --help                # Show all options
```

## Cache Management

The `fortrun` tool uses an intelligent caching system to speed up repeated builds. When developing or testing, you may need to manage the cache:

```bash
# View cache information
fortrun --cache-info
# Output:
# Fortran Cache Information:
#   Cache directory: /home/user/.cache/fortrun
#   Number of files: 487 files
#   Total size: 3.9M

# Clear entire cache
fortrun --clear-cache

# Clear cache and run a file (useful for testing)
fortrun --clear-cache example.f90

# Use custom cache directory
fortrun --cache-dir /tmp/fortrun-cache example.f90
```

**Important for Development**: Always clear the cache when testing new frontend features or compiler flags to ensure you're not using outdated cached files.

## Project Status

**Current**: Production AST Frontend Complete ✅
- ✅ Complete 4-phase compiler architecture (lexer → parser → semantic → codegen)
- ✅ Hindley-Milner type inference system with Algorithm W
- ✅ Smart caching system (2-4x speedup)
- ✅ *lowercase fortran* .lf syntax with automatic type inference
- ✅ Notebook mode with basic output capture (figure capture WIP)
- ✅ Comprehensive test suite (30+ frontend tests)
- ✅ Parallel test runner for faster development

**Next**: FPM Registry integration and advanced language features

---

*"Fortran is the Python of scientific computing - it just doesn't know it yet."*
