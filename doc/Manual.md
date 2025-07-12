title: Manual
---

# Command Line Interface

## Basic Usage

```bash
fortran [OPTIONS] SOURCE_FILE
```

**Examples:**
```bash
fortran hello.f90                    # Run program
fortran -v calculator.f90            # Verbose output  
fortran --cache-dir /tmp calc.f90    # Custom cache
```

## Options

### Verbosity
```bash
-v, --verbose       Show build commands and progress
-vv                 Very verbose (debug output)
```

### Cache Control
```bash
--cache-dir DIR     Use custom cache directory
                    Default: ~/.cache/fortran (Linux/macOS)
                             %LOCALAPPDATA%/fortran/cache (Windows)
```

### Configuration  
```bash
--config-dir DIR    Use custom config directory
                    Default: ~/.config/fortran (Linux/macOS)
                             %LOCALAPPDATA%/fortran/config (Windows)
```

### Help
```bash
-h, --help          Show help message
--version           Show version information
```

# Caching System

The tool caches compiled modules for fast reuse across runs.

## Cache Location

**Default locations:**
- Linux/macOS: `~/.cache/fortran/`
- Windows: `%LOCALAPPDATA%/fortran/cache/`

**Custom location:**
```bash
fortran --cache-dir /tmp/my-cache program.f90
```

## Cache Structure
```
cache/
├── modules/           # Compiled .mod files
├── objects/           # Compiled .o files  
├── executables/       # Built executables
└── timestamps/        # Build metadata
```

## Cache Behavior

- **Automatic invalidation** - Rebuilds when source files change
- **Dependency tracking** - Updates dependent modules automatically
- **Parallel safe** - Uses file locking for concurrent builds
- **Content-based keys** - Uses FPM's digest system for cache keys

**Manual cache management:**
```bash
# Clear cache (manual - no CLI option yet)
rm -rf ~/.cache/fortran/
```

# Module Registry

External dependencies are resolved through a package registry.

## Registry File

**Location:** `~/.config/fortran/registry.toml`

**Example:**
```toml
[packages]

[packages.pyplot-fortran]
git = "https://github.com/jacobwilliams/pyplot-fortran"

[packages.fortplotlib]  
git = "https://github.com/krystophny/fortplotlib"
prefix = "fortplot"  # Any module starting with "fortplot"
```

## Resolution Strategy

1. **Explicit mapping** - Direct module-to-package mapping
2. **Prefix matching** - Module starts with registered prefix  
3. **Underscore inference** - `module_name` → `package-name`
4. **Direct inference** - `module_name` → `package-name`

**Examples:**
```fortran
use pyplot_module        ! → pyplot-fortran package
use fortplot_figure     ! → fortplotlib package (prefix: "fortplot")
use json_module         ! → json-fortran package (inferred)
```

# Architecture

## Execution Flow

```
Source File → Module Scanner → Registry Resolver → FPM Generator → Cache → Execute
```

## Core Components

### Module Scanner (`module_scanner.f90`)
- Parses Fortran source for `use` statements
- Builds dependency graph
- Handles interdependent modules

### Registry Resolver (`registry_resolver.f90`)  
- Maps module names to packages
- Resolves version constraints
- Downloads external dependencies

### FPM Generator (`fpm_generator.f90`)
- Creates dynamic `fmp.toml` files
- Applies modern defaults (implicit none, double precision)
- Configures compiler flags

### Cache Manager (`cache.f90`)
- Content-based cache keys using FPM's digest
- Timestamp-based invalidation
- Cross-platform cache directories

### Runner (`runner.f90`)
- Orchestrates the entire build process
- Handles error reporting
- Manages temporary files

## Modern Defaults

The tool enforces modern Fortran practices:

```toml
# Generated fmp.toml
[fortran]
implicit-typing = false      # Enforces 'implicit none'
implicit-external = false    # No implicit externals
source-form = "free"         # Modern free-form source

[build]
flag = "-fdefault-real-8 -fdefault-double-8"  # Double precision by default
```

# Advanced Usage

## Local Module Development

```bash
# Project structure
my-project/
├── main.f90
├── utils.f90
└── math_helpers.f90
```

```bash
# Automatically finds and builds all modules
fortran main.f90
```

## External Dependencies

**Install packages to registry:**
```toml
# ~/.config/fortran/registry.toml
[packages.my-package]
git = "https://github.com/user/my-package"
tag = "v1.0.0"  # Optional version constraint
```

**Use in code:**
```fortran
program example
    use my_module  ! Automatically resolved and built
    ! ...
end program
```

## Integration with Existing Projects

**Option 1: Direct usage**
```bash
cd existing-fpm-project/
fortran app/main.f90  # Uses existing src/ modules
```

**Option 2: Registry registration**
```bash
# Add local project to registry
echo '[packages.my-lib]' >> ~/.config/fortran/registry.toml
echo 'path = "/path/to/my-lib"' >> ~/.config/fortran/registry.toml
```

## Performance Considerations

- **First run**: Downloads and builds dependencies (slower)
- **Subsequent runs**: Uses cached modules (fast)
- **Source changes**: Rebuilds only affected modules
- **Cache size**: Grows over time, manual cleanup needed

**Benchmark results:**
```bash
# First run (with dependencies)
time fortran plotting_example.f90  # ~30s

# Cached run  
time fortran plotting_example.f90  # ~2s

# Modified source (partial rebuild)
time fortran plotting_example.f90  # ~5s
```

# Troubleshooting

## Common Issues

**Module not found:**
```
Error: Module 'my_module' not found in registry
```
**Solution:** Add package to `~/.config/fortran/registry.toml`

**Build failures:**
```bash
# Use verbose mode to see compiler errors
fortran -vv problematic.f90
```

**Cache corruption:**
```bash
# Clear cache and retry
rm -rf ~/.cache/fortran/
fortran myprogram.f90
```

**Permission errors:**
```bash
# Check cache directory permissions
ls -la ~/.cache/fortran/
```

## Debug Output

**Verbose mode (`-v`):**
```bash
$ fortran -v hello.f90
[INFO] Scanning for modules in hello.f90
[INFO] No external dependencies found
[INFO] Generating fmp.toml
[INFO] Building with FMP...
[INFO] Execution completed
```

**Very verbose mode (`-vv`):**
```bash
$ fortran -vv hello.f90
[DEBUG] Cache directory: /home/user/.cache/fortran
[DEBUG] Config directory: /home/user/.config/fortran  
[DEBUG] Source file: hello.f90
[DEBUG] Module dependencies: []
[DEBUG] Cache key: abc123def456
[DEBUG] FMP command: fmp run --flag "-fdefault-real-8 -fdefault-double-8"
[DEBUG] Build output: [compiler messages]
```

## Error Messages Guide

| Error | Meaning | Solution |
|-------|---------|----------|
| `Module 'X' not found` | Missing package in registry | Add to registry.toml |
| `FMP build failed` | Compiler error | Check source code, use `-vv` |
| `Cache lock timeout` | Parallel build conflict | Wait or clear locks |
| `Permission denied` | Cache directory access | Check permissions |

## Getting Help

- Use `fortran --help` for CLI reference
- Check [[Examples]] for working code samples  
- Report issues at https://github.com/krystophny/fortran/issues
- Enable verbose mode (`-vv`) when reporting bugs
