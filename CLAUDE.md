# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

This project develops a command-line tool called `fortran` that **makes Fortran as easy as Python**. The tool enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM, with opinionated modern defaults and zero configuration.

## Build System

This project uses FPM (Fortran Package Manager) as its build system.

### Common Commands

```bash
# Build the project
fpm build

# Run the main application
./build/gfortran_*/app/fortran example.f90

# Run tests
fmp test

# Run specific test
fpm test test_name

# Build with release optimizations
fpm build --profile release

# Clean build artifacts
fpm clean
```

## Project Architecture

This is a Fortran project following standard FPM directory structure:

- `src/` - Library modules that provide reusable functionality
- `app/` - Executable programs that use the library modules  
- `test/` - Test programs with comprehensive coverage
- `example/` - Example programs organized by feature

### Key Architectural Patterns

1. **Module-based Organization**: Core functionality is organized into modules in `src/`:
   - `cli.f90` - Command-line argument parsing
   - `runner.f90` - Main execution logic
   - `module_scanner.f90` - Module dependency detection
   - `registry_resolver.f90` - Package registry resolution
   - `fpm_generator.f90` - Dynamic fpm.toml generation
   - `cache.f90` - OS-specific caching
   - `config.f90` - Configuration directory management

2. **Explicit Typing**: The codebase enforces `implicit none` throughout, requiring all variables to be explicitly declared.

3. **Private-by-Default**: Modules use private visibility by default, explicitly marking public interfaces.

4. **Test-Driven Development**: Every feature has comprehensive test coverage.

## Development Configuration

The `fpm.toml` configuration enforces:
- No implicit typing (`implicit-typing = false`)
- No implicit externals (`implicit-external = false`)
- Free-form source code (`source-form = "free"`)
- Automatic discovery of executables and tests
- Auto-examples disabled (`auto-examples = false`) due to external dependencies

## Testing Strategy

Comprehensive test coverage includes:

### Unit Tests
- `test_module_scanner.f90` - Module dependency detection
- `test_registry_resolver.f90` - Package registry resolution
- `test_fpm_generator.f90` - fpm.toml generation
- `test_cache.f90` - Caching functionality
- `test_cli.f90` - Command-line argument parsing

### Integration Tests
- `test_examples.f90` - Runs all examples and validates output
- `test_verbose.f90` - Verbose mode functionality

### System Tests
- `test_cli_system.f90` - End-to-end CLI testing with real commands

To run specific tests:
```bash
fpm test test_name
```

## Implementation Status

### ✅ **Phase 1 Complete**: Foundation
- ✅ Basic CLI with comprehensive argument parsing (`--help`, `-v`, `-vv`, `--verbose`, `--cache-dir`, `--config-dir`)
- ✅ Local module dependency resolution with interdependent module support
- ✅ FPM integration with modern defaults (implicit none, double precision)
- ✅ OS-specific caching and configuration management
- ✅ Package registry with smart module resolution (prefix matching, underscore inference)
- ✅ Comprehensive test coverage (unit, integration, system tests)

### 🚧 **Phase 2 In Progress**: Enhanced Features
- ✅ Interdependent local modules example
- ✅ Custom cache/config directories
- ✅ Organized example structure with documentation
- ⚠️ Advanced file handling (subdirectories, relative imports)
- ⚠️ Registry enhancements (version constraints, validation)
- ⚠️ Error handling improvements

## Modern Defaults (Opinionated Design)

The tool enforces modern Fortran practices by default:

1. **`implicit none`** - Enforced automatically via `fpm.toml`
2. **Double precision** - `real` defaults to `real(8)` via compiler flags
3. **Free form** - Modern source format
4. **Standard compliance** - Generates Fortran 2018 code

### Compiler Flags Applied
```bash
--flag "-fdefault-real-8 -fdefault-double-8"
```

## Module Registry System

The tool includes a registry that maps Fortran module names to their respective packages, designed to be compatible with the [FPM Registry](https://github.com/fortran-lang/fpm-registry) format.

### Registry Format

```toml
# ~/.config/fortran/registry.toml
[packages]

[packages.fortplotlib]
git = "https://github.com/krystophny/fortplotlib"
prefix = "fortplot"  # Any module starting with "fortplot"

[packages.pyplot-fortran]
git = "https://github.com/jacobwilliams/pyplot-fortran"
# pyplot_module -> pyplot-fortran (underscore inference)
```

### Module Resolution Strategy

1. **Explicit mappings**: Check direct module-to-package mappings
2. **Custom prefixes**: Check if module starts with a registered prefix
3. **Automatic inference**: 
   - If module contains underscore: `module_name` → `package-name` (part before first `_`)
   - If no underscore: `module_name` → `package-name` (the module name itself)

### Configuration Directories

The tool uses standard OS directories:
- **Config**: `~/.config/fortran/` (Linux/macOS) or `%LOCALAPPDATA%/fortran/config/` (Windows)
- **Cache**: `~/.cache/fortran/` (Linux/macOS) or `%LOCALAPPDATA%/fortran/cache/` (Windows)
- **Registry**: `~/.config/fortran/registry.toml`

## Example Structure

Examples are organized into subdirectories:
- `example/hello/` - Basic hello world
- `example/calculator/` - Local module usage
- `example/precision/` - Modern precision defaults
- `example/interdependent/` - Complex interdependent modules
- `example/plotting/` - External dependencies (disabled)

Each example includes:
- Source files
- README.md with documentation
- Test validation in `test_examples.f90`

## Development Principles

1. **Opinionated for Good**: Make choices that help users adopt modern Fortran practices
2. **Python-like Experience**: `fortran mycode.f90` should be as easy as `python mycode.py`
3. **Test-Driven Development**: Write tests first, comprehensive coverage
4. **Zero Configuration**: Just write code and run it
5. **Gradual Adoption**: Works with existing FPM packages and build systems

## Key Implementation Decisions

1. **FPM Integration**: Uses FPM CLI for building, generates dynamic `fpm.toml` files
2. **Caching Strategy**: OS-specific cache directories with timestamp-based invalidation
3. **Registry Design**: TOML-based, FPM-compatible with smart resolution rules
4. **Modern Defaults**: Compiler flags and fpm.toml settings enforce best practices

## Development Notes

- **FPM Fork**: We use the standard FPM, keeping changes minimal or separate
- **Caching**: Part of `fortran` tool, not FPM itself
- **Testing**: Every feature must have tests before merging
- **Documentation**: Examples serve as both documentation and tests

## Future Roadmap

- **Phase 3**: Smart caching with content hashing
- **Phase 4**: Cross-package support and performance optimization  
- **Phase 5**: Integration with official FPM registry
- **Phase 6**: Simplified Fortran preprocessor (`.f` files)
- **Phase 7**: Type inference system

**Goal**: Make Fortran development as seamless as Python, where you can just run a file without worrying about compilation, linking, or dependency management.

*"Fortran is the Python of scientific computing - it just doesn't know it yet."*