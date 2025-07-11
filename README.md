# ![fortran logo](media/logo.png)

**Make Python Fortran again.** - A command-line tool that enables running Fortran programs as easily as Python scripts, with automatic dependency resolution, modern defaults, and zero configuration.

## Overview

The `fortran` command works like `python` but for `.f90` files. Give it a valid Fortran program file, and it will automatically:
- **Resolve module dependencies** from local files and package registries
- **Apply modern defaults** (implicit none, double precision, etc.)
- **Build all required modules** using FPM (Fortran Package Manager)  
- **Cache builds** for fast subsequent runs
- **Execute the resulting program** transparently

## Quick Start

```bash
# Run any Fortran program instantly
fortran hello.f90

# Works with local modules automatically
fortran calculator.f90  # Uses math_module.f90 in same directory

# Complex interdependent modules? No problem!
fortran example/interdependent/main.f90

# External dependencies resolved from registry
fortran plot_demo.f90  # Automatically fetches pyplot-fortran

# Verbose output for debugging
fortran -v myprogram.f90
fortran --verbose 2 myprogram.f90
```

## ✨ Features

### 🚀 **Zero Configuration**
- No Makefiles, no build scripts, no project setup
- Just write Fortran code and run it
- Automatic dependency detection and resolution

### 🎯 **Opinionated Modern Defaults**
- **`implicit none`** enforced automatically
- **Double precision** (`real(8)`) as default for `real` variables
- **Free form** source format
- **Modern compiler flags** applied automatically

### 📦 **Smart Dependency Resolution**
- **Local modules**: Automatically includes `.f90` files from same directory
- **Package registry**: Resolves external modules to git repositories
- **Interdependent modules**: Handles complex dependency chains
- **Caching**: Builds cached for fast subsequent runs

### 🛠️ **Comprehensive CLI**
- **Help**: `--help`, `-h`
- **Verbose modes**: `-v`, `-vv`, `--verbose 1`, `--verbose 2`
- **Custom directories**: `--cache-dir`, `--config-dir`
- **Error handling**: Clear error messages and suggestions

## Examples

### Basic Hello World
```fortran
! hello.f90
program hello
  print *, "Hello from Fortran!"
end program hello
```
```bash
fortran hello.f90
# Output: Hello from Fortran!
```

### Local Module Usage
```fortran
! math_utils.f90
module math_utils
  implicit none
  public :: add, multiply
contains
  function add(a, b) result(c)
    real :: a, b, c
    c = a + b
  end function add
  
  function multiply(a, b) result(c)
    real :: a, b, c
    c = a * b
  end function multiply
end module math_utils

! calculator.f90
program calculator
  use math_utils, only: add, multiply
  implicit none
  
  real :: x, y
  x = 5.0
  y = 3.0
  
  print *, 'Sum:', add(x, y)
  print *, 'Product:', multiply(x, y)
end program calculator
```
```bash
fortran calculator.f90
# Automatically finds and builds math_utils.f90
# Output: Sum: 8.0
#         Product: 15.0
```

### Complex Interdependent Modules
The tool handles complex dependency chains automatically:
```
main.f90
├── geometry.f90
│   ├── constants.f90
│   └── geometry.f90 (internal functions)
└── input_output.f90
    └── constants.f90
```

See `example/interdependent/` for a complete working example.

## Installation

**Prerequisites:**
- Modern Fortran compiler (gfortran 9+ recommended)
- FPM (Fortran Package Manager)

**Build from source:**
```bash
git clone https://github.com/krystophny/fortran.git
cd fortran
fpm build
# Copy to your PATH
cp build/gfortran_*/app/fortran ~/.local/bin/
```

## Configuration

The tool uses standard OS directories:
- **Config**: `~/.config/fortran/` (Linux/macOS)
- **Cache**: `~/.cache/fortran/` (Linux/macOS)
- **Registry**: `~/.config/fortran/registry.toml`

### Package Registry

The registry maps module names to git repositories:
```toml
[packages.fortplotlib]
git = "https://github.com/krystophny/fortplotlib"
prefix = "fortplot"  # Any module starting with "fortplot"

[packages.pyplot-fortran]
git = "https://github.com/jacobwilliams/pyplot-fortran"
# pyplot_module -> pyplot-fortran (underscore inference)
```

## Design Philosophy

### **Opinionated for Good**
We make **opinionated design choices**:

1. **Modern defaults by default** - No more `implicit none` boilerplate
2. **Double precision by default** - Scientific computing needs precision
3. **Zero configuration** - Just write code and run it
4. **Automatic dependency resolution** - No manual dependency management
5. **Comprehensive testing** - Every feature is tested

### **Python-like Developer Experience**
- **Instant execution**: `fortran mycode.f90` (like `python mycode.py`)
- **Automatic imports**: Local modules detected automatically
- **Package management**: External dependencies resolved from registries
- **Error handling**: Clear, helpful error messages

### **Fortran's Strengths, Python's Ease**
- **Performance**: Full Fortran performance, no runtime overhead
- **Compatibility**: Generates standard Fortran 2018 code
- **Ecosystem**: Works with existing FPM packages and tools
- **Gradual adoption**: Mix with existing build systems

## Development Status

### ✅ **Phase 1 Complete**: Foundation (v0.1.0)
- ✅ Basic CLI with comprehensive argument parsing
- ✅ Local module dependency resolution
- ✅ FPM integration with modern defaults
- ✅ OS-specific caching and configuration
- ✅ Package registry with smart module resolution
- ✅ Comprehensive test coverage (unit, integration, system tests)

### ✅ **Phase 2 Complete**: Enhanced Features
- ✅ Interdependent local modules
- ✅ Custom cache/config directories
- ✅ Organized example structure
- ✅ Registry enhancements (version constraints, validation)
- ✅ Error handling improvements
- ✅ Multiple modules from same package support
- ✅ Conflicting dependencies resolution

### 🚧 **Phase 3 In Progress**: Smart Caching
- ✅ Cache directory structure and management
- ✅ FPM API integration for content-based hashing
- ✅ Cache key generation using FPM's digest system
- ⚠️ Build artifact caching (Phase 3.2)
- ⚠️ Cache retrieval and validation (Phase 3.3)

### 🔮 **Future Phases**: Advanced Features
- **Phase 4**: Cross-package support and performance optimization
- **Phase 5**: Integration with official FPM registry
- **Phase 6**: Simplified Fortran preprocessor (`.f` files)
- **Phase 7**: Type inference system

## Contributing

This project follows **TDD** (Test-Driven Development) and **SOLID** principles:
- Write tests first
- Keep changes small and focused
- Comprehensive test coverage for all features
- Clear documentation and examples

## License

MIT License - see LICENSE file for details.

---

**Goal**: Make Fortran development as seamless as Python, where you can just run a file without worrying about compilation, linking, or dependency management.

*"Fortran is the Python of scientific computing - it just doesn't know it yet."*
