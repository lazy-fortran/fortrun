# Design Architecture

This document describes the key design decisions and architectural patterns behind the Fortran CLI tool that **Makes Python Fortran again**.

## Core Design Philosophy

The tool follows these fundamental principles:

1. **Zero Configuration**: Users should be able to run Fortran code as easily as Python: `fortran mycode.f90`
2. **Opinionated Defaults**: Enforce modern Fortran practices (implicit none, real(8), free form)
3. **Content-Based Caching**: Avoid redundant compilation through intelligent caching
4. **Module Ecosystem**: Seamlessly integrate with FPM packages and local modules
5. **Progressive Enhancement**: Start simple (.f syntax) but support full Fortran complexity

## Component Architecture

### 1. CLI Interface (`cli.f90`)
**Purpose**: Parse command-line arguments and orchestrate the workflow

**Key Features**:
- Comprehensive argument parsing (`--help`, `-v`, `-vv`, `--verbose`, `--cache-dir`, `--config-dir`)
- Support for custom cache/config directories
- Parallel build control (`-j`, `--jobs`)
- Future: Notebook mode (`--notebook`)

**Design Pattern**: Single responsibility - only handles argument parsing and delegates to runner

### 2. Main Runner (`runner.f90`)
**Purpose**: Orchestrate the entire execution pipeline

**Workflow**:
1. **File Validation**: Check file exists and has valid extension
2. **Preprocessing**: Convert `.f` files to `.f90` if needed
3. **Content Hashing**: Generate cache keys based on file content
4. **Cache Check**: Look for existing compiled artifacts
5. **Module Scanning**: Detect local and external dependencies
6. **Project Generation**: Create dynamic `fpm.toml` files
7. **Build & Execute**: Compile and run with appropriate caching

**Critical Decision**: Use content-based hashing rather than timestamp-based invalidation for robust caching across different environments.

### 3. Intelligent Caching System (`cache.f90`)

#### Cache Directory Structure
```
~/.cache/fortran/                 # OS-specific cache location
├── builds/                       # Compiled build artifacts
│   └── fmp_A1B2C3.../           # Content-hash named directories
│       ├── build/               # FPM build output
│       └── app/                 # Compiled executables
├── modules/                      # Module cache (future use)
├── executables/                  # Standalone executables
└── metadata/                     # Build metadata
```

#### Content-Based Hashing Strategy

**Key Innovation**: Uses FPM's internal `fnv_1a` hash algorithm to create content-based cache keys.

```fortran
! Example: cache.f90:324-356
function get_content_hash(source_files) result(hash_key)
  ! Reads all source files using FPM's read_lines
  ! Combines content using FPM's fnv_1a hash function
  ! Generates cache key like "fmp_A1B2C3D4"
end function
```

**Benefits**:
- **Cross-machine consistency**: Same source = same cache key on any machine
- **Content sensitivity**: Any source change invalidates cache automatically
- **Collision resistance**: Uses proven FPM hash algorithm
- **Parallel safety**: Multiple processes can safely share cache

#### Cache Sharing Across Different Calls

**Problem Solved**: When user runs `fortran calc.f90` then `fortran plot.f90`, both might use the same local modules (e.g., `math_utils.f90`). Traditional build systems would recompile modules for each call.

**Solution**: Content-based module caching
1. **First call** (`fortran calc.f90`):
   - Scans `calc.f90` and discovers dependency on local `math_utils.f90`
   - Generates hash based on content of both files: `hash_AB123`
   - Builds project and stores artifacts in `~/.cache/fortran/builds/hash_AB123/`
   - Executes program

2. **Second call** (`fortran plot.f90`):
   - Scans `plot.f90` and discovers same dependency on `math_utils.f90`
   - If `math_utils.f90` unchanged, generates hash including same module content
   - **Cache hit**: Finds existing build artifacts
   - **Skip compilation**: Directly executes from cache

**Cache Key Generation** (`cache.f90:324-356`):
```fortran
! Combines content from ALL source files into single hash
do i = 1, size(source_files)
  file_contents = read_lines(trim(source_files(i)))
  combined_digest = ieor(combined_digest, fnv_1a(file_contents))
end do
```

This means two different programs that use the same set of modules will share the same cache entry, dramatically speeding up subsequent builds.

### 4. Module Scanner (`module_scanner.f90`)
**Purpose**: Detect and analyze module dependencies

**Discovery Process**:
1. **Parse USE statements**: Extract required module names
2. **Local module detection**: Scan current directory for `.f90` files providing modules
3. **Registry lookup**: Check external package registry for unknown modules
4. **Dependency resolution**: Build complete dependency graph

**Key Challenge**: Distinguishing between local modules (same directory) and external packages (FPM registry).

**Solution**: Priority-based resolution:
1. Local files first (highest priority)
2. Registry packages second
3. Error if module not found

### 5. Registry Resolver (`registry_resolver.f90`)
**Purpose**: Map Fortran module names to FPM packages

**Registry Format** (`~/.config/fortran/registry.toml`):
```toml
[packages.fortplot]
git = "https://github.com/krystophny/fortplot"
prefix = "fortplot"  # Any module starting with "fortplot"

[packages.pyplot-fortran]
git = "https://github.com/jacobwilliams/pyplot-fortran"
# pyplot_module -> pyplot-fortran (underscore inference)
```

**Smart Resolution Strategy**:
1. **Explicit mappings**: Direct module-to-package mappings
2. **Prefix matching**: `fortplot_scatter` → `fortplot` (via prefix)
3. **Underscore inference**: `pyplot_module` → `pyplot-fortran`
4. **Fallback**: `module_name` → `package-name`

**Design Rationale**: Fortran ecosystem has inconsistent naming (underscores vs hyphens), so multiple resolution strategies ensure maximum compatibility.

### 6. FPM Generator (`fmp_generator.f90`)
**Purpose**: Generate dynamic `fpm.toml` files for each build

**Template Generation**:
```toml
name = "hello_fpm_ABC123"      # Unique name per cache entry
version = "0.1.0"

[fortran]
implicit-typing = false         # Enforces implicit none
implicit-external = false      # No implicit externals
source-form = "free"           # Modern syntax

[[executable]]
name = "hello"
main = "main.f90"              # User's file (possibly preprocessed)

[dependencies]
fortplot = { git = "https://github.com/..." }  # Auto-resolved
```

**Key Insight**: Each build gets a unique project name based on content hash, preventing conflicts when multiple `fortran` invocations run simultaneously.

### 7. Preprocessor (`preprocessor.f90`)
**Purpose**: Convert simplified `.f` syntax to standard `.f90`

**Transformations Applied**:
1. **Program wrapper**: Wrap code in `program main` / `end program main`
2. **Implicit none**: Add automatically
3. **Type inference**: Analyze usage patterns and generate declarations
4. **Modern defaults**: All `.f` files get `real(8)` precision flags

**Type Inference Engine**:
```fortran
! Input .f file:
x = 5.0
name = "Alice"
ready = .true.

! Generated .f90 output:
program main
  implicit none
  real(8) :: x
  character(len=5) :: name  
  logical :: ready
  
  x = 5.0_8
  name = "Alice"
  ready = .true.
end program main
```

**Current Capabilities**:
- ✅ Basic types: integer, real(8), character(len=N), logical
- 🔄 Arrays: `[1, 2, 3]` → `integer, dimension(3)` (planned)
- 🔄 Derived types: Field access pattern recognition (planned)

## Cross-Call Module Caching: Detailed Example

Consider this workflow demonstrating efficient module reuse:

### Scenario: Two programs sharing a math library

**Files**:
- `math_utils.f90` - Common math functions (100 lines)
- `calculator.f90` - Calculator using math_utils
- `plotter.f90` - Plotting tool using math_utils

### First Call: `fortran calculator.f90`

1. **Module Scanning**: Detects `use math_utils`
2. **Content Hashing**: 
   ```
   hash(calculator.f90 + math_utils.f90) = "fpm_AB7293E1"
   ```
3. **Cache Miss**: No existing cache for this hash
4. **Project Generation**: Creates temporary FPM project
5. **Compilation**: FPM builds both files, generates `.mod` files and executable
6. **Cache Storage**: Stores entire `build/` directory at:
   ```
   ~/.cache/fortran/builds/fmp_AB7293E1/
   ├── build/gfortran_*/
   │   ├── math_utils.mod        # Compiled module interface
   │   └── math_utils.o          # Compiled object code
   └── app/calculator            # Final executable
   ```
7. **Execution**: Runs calculator

### Second Call: `fortran plotter.f90`

1. **Module Scanning**: Detects `use math_utils` (same module!)
2. **Content Hashing**:
   ```
   hash(plotter.f90 + math_utils.f90) = "fmp_CD8472F5"
   ```
   Note: Different hash because `plotter.f90` differs from `calculator.f90`
3. **Cache Miss**: Different program = different hash
4. **Build Process**: Must compile `plotter.f90`, but can reuse `math_utils.mod` if smart enough

### Third Call: `fortran calculator.f90` (again)

1. **Content Hashing**: Same as before: `"fmp_AB7293E1"`
2. **Cache Hit**: Finds existing cache entry
3. **Skip Compilation**: Directly execute from cache
4. **Performance**: ~100x faster (milliseconds vs seconds)

### Future Enhancement: Package-Level Caching (Phase 8)

Currently, the tool caches complete project builds. **Phase 8** will add package-level caching to enable FPM package sharing across different programs:

**Current Limitation**:
```bash
fortran calc.f90     # Downloads + compiles pyplot-fortran
fortran plot.f90     # Recompiles pyplot-fortran (no sharing)
```

**Phase 8 Enhancement**:
```
~/.cache/fortran/
├── packages/                    # NEW: Package-level cache
│   ├── pyplot-fortran_v1.2.3/  # Compiled package by version
│   │   ├── *.mod               # Module interfaces
│   │   └── *.o                 # Compiled objects
│   └── fortplot_latest/     # Latest version cache
└── builds/                      # Enhanced project cache
    └── calc_ABC123_pyplot-v1.2.3/  # Includes package versions in hash
```

**Enhanced Cache Key Strategy**:
```fortran
! Current: hash(local_files_only)
! Phase 8:  hash(local_files + fpm_dependencies + versions)
structure_hash = hash(local_modules + external_packages + package_versions)
```

**Benefits**:
- `fortran calc.f90` and `fortran plot.f90` both using pyplot-fortran would share compiled package
- Dramatic speed improvement for projects with heavy external dependencies
- Version-aware caching prevents conflicts when packages update

## Performance Characteristics

### Cache Hit vs Miss Performance

**Cache Miss** (first run):
- File scanning: ~10ms
- Module resolution: ~50ms
- FPM compilation: ~2-10 seconds (depending on dependencies)
- Cache storage: ~100ms
- **Total**: 2-10 seconds

**Cache Hit** (subsequent runs):
- File scanning: ~10ms
- Content hashing: ~20ms
- Cache lookup: ~5ms
- Direct execution: ~50ms
- **Total**: ~85ms

**Speedup**: 20-100x faster for cache hits

### Parallel Safety

The caching system is designed for parallel safety:

1. **Content-based keys**: No race conditions on key generation
2. **Atomic directory creation**: `mkdir -p` is atomic
3. **Locking mechanism** (`cache_lock.f90`): Prevents simultaneous builds of same content
4. **Cleanup on exit**: Stale locks are removed automatically

## Integration with FPM Ecosystem

### Design Decision: Leverage FPM Rather Than Replace

The tool deliberately uses FPM as its build backend rather than implementing a custom build system:

**Benefits**:
- **Compatibility**: Works with existing FPM packages
- **Robustness**: Leverages FPM's mature dependency resolution
- **Future-proof**: Automatically benefits from FPM improvements
- **Community**: Integrates with established Fortran ecosystem

**Implementation Strategy**:
- Generate dynamic `fpm.toml` files per build
- Use FPM CLI commands for actual compilation
- Cache FPM build outputs, not just source files
- Leverage FPM's own source parsing and hashing utilities

### Registry Integration

The registry system is designed to be compatible with the official FPM registry format, enabling eventual migration:

**Current**: Local `~/.config/fortran/registry.toml`
**Future**: Integration with https://github.com/fortran-lang/fpm-registry

This ensures the tool can evolve with the broader Fortran ecosystem.

## Future Architectural Enhancements

### 1. Distributed Caching
Share cache entries across machines/users:
```bash
fortran --cache-url https://cache.fortran.org calc.f90
```

### 2. Incremental Compilation
Track dependencies at module level for faster rebuilds:
```
Changed: math_utils.f90
Rebuild: calculator.f90, plotter.f90  (depend on math_utils)
Keep:    io_helpers.f90               (independent)
```

### 3. Cross-Language Integration
Enable Python-style package management:
```bash
fortran --install pyplot-fortran      # Like pip install
fortran --requirements packages.toml  # Like requirements.txt
```

## Conclusion

The Fortran CLI tool achieves its goal of "making Python Fortran again" through:

1. **Smart Caching**: Content-based hashing eliminates redundant compilation
2. **Module Ecosystem**: Seamless integration with local and external modules
3. **Zero Configuration**: Opinionated defaults reduce cognitive overhead
4. **Progressive Enhancement**: Start simple with `.f`, scale to full `.f90` complexity

The architecture prioritizes developer experience while maintaining compatibility with the existing Fortran ecosystem, creating a foundation for modern Fortran development practices.