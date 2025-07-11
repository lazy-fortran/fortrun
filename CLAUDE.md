# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

This project develops a command-line tool called `fortran` that works like `python` but for `.f90` files. The tool enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM.

## Build System

This project uses FPM (Fortran Package Manager) as its build system.

### Common Commands

```bash
# Build the project
fpm build

# Run the main application
fpm run

# Run tests
fpm test

# Build with release optimizations
fpm build --profile release

# Run a specific executable
fpm run --target app_name

# Clean build artifacts
fpm clean
```

## Project Architecture

This is a Fortran project following standard FPM directory structure:

- `src/` - Library modules that provide reusable functionality
- `app/` - Executable programs that use the library modules
- `test/` - Test programs (currently contains placeholder)

### Key Architectural Patterns

1. **Module-based Organization**: Core functionality is organized into modules in `src/`, with the main module being `fortran` that exports public procedures.

2. **Explicit Typing**: The codebase enforces `implicit none` throughout, requiring all variables to be explicitly declared.

3. **Private-by-Default**: Modules use private visibility by default, explicitly marking public interfaces.

## Development Configuration

The `fpm.toml` configuration enforces:
- No implicit typing (`implicit-typing = false`)
- No implicit externals (`implicit-external = false`)
- Free-form source code (`source-form = "free"`)
- Automatic discovery of executables and tests

## Testing

Tests should be placed in the `test/` directory. FPM will automatically discover and compile them. Currently, `test/check.f90` is a placeholder that needs implementation.

To run a specific test:
```bash
fpm test --target test_name
```

## Implementation Strategy

### Phase 1: Basic Implementation
- Create a CLI tool that accepts a `.f90` file as input
- Use FPM to build the file with its dependencies
- Execute the resulting binary
- Initial version rebuilds on every run (no caching)

### Phase 2: Caching System
- Implement a caching mechanism to avoid recompilation
- Cache location should be in a system-appropriate directory (e.g., `~/.cache/fortran/`)
- Use file timestamps and content hashes to detect changes
- Store compiled modules and executables in the cache

### Phase 3: Cross-Package Support
- Extend to work across different packages like Cargo
- May require extending FPM's functionality
- Support for package registries and version management

## Key Implementation Decisions

1. **FPM Integration**: Can use either FPM library API or command-line interface. Choose based on:
   - API for fine-grained control and better error handling
   - CLI for simpler operations and easier maintenance

2. **Cache Structure**: Design cache to store:
   - Compiled modules (`.mod` files)
   - Object files (`.o` files)
   - Executable binaries
   - Dependency graphs
   - Build metadata (timestamps, hashes)

3. **Dependency Resolution**: Leverage FPM's existing dependency resolution capabilities while adding:
   - Dynamic dependency detection from single `.f90` files
   - Module search paths
   - External package resolution

## Module Registry

The tool includes a registry that maps Fortran module names to their respective packages, designed to be compatible with the [FPM Registry](https://github.com/fortran-lang/fpm-registry) format. The registry uses TOML format.

### Registry Format

The registry follows FPM's package namespace structure:

```toml
# registry.toml
[packages]

[packages.json-fortran]
version = "8.3.0"
git = "https://github.com/jacobwilliams/json-fortran"
modules = ["json_module", "json_kinds", "json_parameters"]

[packages.stdlib]
version = "0.2.0" 
git = "https://github.com/fortran-lang/stdlib"
modules = ["stdlib_io", "stdlib_kinds", "stdlib_string_type", "stdlib_ascii"]

[packages.fftpack]
version = "1.0.0"
git = "https://github.com/fortran-lang/fftpack"
modules = ["fftpack"]
```

### Module Lookup Table

For fast module-to-package resolution, we maintain a derived lookup table:

```toml
# module_index.toml (auto-generated)
[modules]
json_module = "json-fortran"
json_kinds = "json-fortran"
stdlib_io = "stdlib"
stdlib_kinds = "stdlib"
fftpack = "fftpack"
```

### Registry Integration

1. **Local Registry**: Store package definitions in `registry.toml`
2. **Module Index**: Auto-generate `module_index.toml` for quick lookups
3. **FPM Registry Sync**: Optionally pull package info from the official FPM registry
4. **Namespace Support**: Follow FPM's namespace convention (e.g., `fortran-lang/stdlib`)

### Usage Flow

When encountering `use module_name`:
1. Check local source files
2. Look up module in `module_index.toml`
3. Find package details in `registry.toml`
4. Use FPM to fetch and build the dependency
5. Cache the built package for future use

This approach maintains compatibility with the FPM ecosystem while enabling Python-like import behavior.