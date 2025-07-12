@Note
This is the documentation for the `fortran` command-line tool that enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM.
@endnote

## Features

- **Zero-configuration execution**: Just run `fortran mycode.f90`
- **Automatic dependency resolution**: Finds and builds required modules
- **Smart caching**: Reuses compiled modules across runs
- **Modern defaults**: Enforces best practices (implicit none, double precision)
- **FPM integration**: Leverages the Fortran Package Manager ecosystem

## Quick Start

```bash
# Run a simple Fortran program
fortran hello.f90

# Run with verbose output
fortran -v myprogram.f90

# Use custom cache directory
fortran --cache-dir /tmp/fortran-cache myprogram.f90
```

## Architecture

The tool consists of several key modules:

- **CLI**: Command-line argument parsing
- **Runner**: Main execution logic
- **Module Scanner**: Detects module dependencies
- **Registry Resolver**: Maps modules to packages
- **FPM Generator**: Creates dynamic fpm.toml files
- **Cache**: Manages compiled module cache

## Contributing

Contributions are welcome! Please see our [GitHub repository](https://github.com/krystophny/fortran) for more information.
