# fortran

A command-line tool that enables running Fortran programs as easily as Python scripts, with automatic dependency resolution and caching.

## Overview

The `fortran` command works like `python` but for `.f90` files. Give it a valid Fortran program file, and it will automatically:
- Resolve module dependencies
- Build all required modules using FPM (Fortran Package Manager)
- Execute the resulting program

## Usage (planned)

```bash
fortran myprogram.f90
```

## Development Roadmap

### Phase 1: Basic Implementation (Current)
- Implement basic command that builds and runs a single `.f90` file
- Use FPM to handle module dependencies
- Build is performed on every run (no caching yet)

### Phase 2: Caching Mechanism
- Develop a real caching system to avoid recompilation for unchanged `.f90` files
- Cache compiled modules and executables
- Detect when recompilation is needed based on file timestamps and dependencies

### Phase 3: Cross-Package Support
- Extend functionality to work across different packages
- Similar to how Cargo handles Rust dependencies
- May require extending FPM itself to support this workflow

## Implementation Details

The tool can be implemented using:
- FPM library API calls for programmatic access
- FPM command-line interface for simpler operations
- A hybrid approach choosing the best method for each task

## Goal

Make Fortran development as seamless as Python, where you can just run a file without worrying about compilation, linking, or dependency management.
