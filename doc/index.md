# fortran - Makes Python Fortran again

Welcome to the documentation for the `fortran` command-line tool.

## Overview

The `fortran` tool enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM (Fortran Package Manager).

## Key Features

- **Zero Configuration**: Just run `fortran mycode.f90` - no build files needed
- **Automatic Dependency Resolution**: Finds and builds required modules from local files or FPM registry
- **Smart Caching**: Reuses compiled modules across runs for fast execution
- **Modern Defaults**: Enforces best practices (implicit none, double precision)
- **Cross-Platform**: Works on Linux, macOS, and Windows

## Quick Start

```bash
# Run a simple Fortran program
fortran hello.f90

# Run with verbose output
fortran -v myprogram.f90

# Use custom cache directory
fortran --cache-dir /tmp/fortran-cache myprogram.f90
```

## Documentation

- [[CLI|Command-Line Interface]] - All command-line options
- [[Architecture|Architecture]] - How the tool works internally
- [[Examples|Examples]] - Usage examples and tutorials
- [[Registry|Module Registry]] - How to configure external modules
- [[Contributing|Contributing]] - How to contribute to the project
