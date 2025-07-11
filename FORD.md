---
project: fortran
summary: Makes Python Fortran again - Run Fortran programs directly without manual compilation
author: krystophny
src_dir: ./src
         ./app
output_dir: ./build/doc
exclude_dir: ./build
             ./test
page_dir: ./doc
project_github: https://github.com/krystophny/fortran
project_download: https://github.com/krystophny/fortran/releases
project_website: https://krystophny.github.io/fortran/
display: public
         protected
source: true
graph: true
search: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
---

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