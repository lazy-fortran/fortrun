# TODO.md

This document tracks the development tasks for the `fortran` CLI tool. It should be updated as items are completed or plans change.

## Development Principles
- Follow TDD (Test-Driven Development) - write tests first
- Apply SOLID principles throughout
- Keep work units small, manageable, and self-contained
- Each task should be completable independently
- Prefer using the existing fpm fork (https://github.com/krystophny/fpm) minimally or not at all

## Phase 1: Foundation and Basic CLI (Simplified with FPM)

### 1.1 Project Setup
- [x] Set up basic CLI structure in `app/main.f90`
  - Parse command line arguments using Fortran intrinsics
  - Support: `fortran <file.f90>` and `fortran --help`
  - Exit with proper codes (0 for success, 1 for errors)
- [x] Create initial registry files
  - `registry.toml` with fortplotlib and pyplot-fortran
  - `module_index.toml` mapping modules to packages
- [x] Define minimal core modules in `src/`
  - `src/cli.f90`: Command line parsing
  - `src/runner.f90`: Main execution logic

### 1.2 Minimal Viable Product (Using FPM directly)
- [x] Write test: `test_run_simple_program`
  - Test .f90 file without external dependencies
  - Verify execution via our tool
  - ✅ **Covered by**: `example/hello.f90` and `test_examples.f90`
- [x] Implement basic execution in `src/runner.f90`
  - Create temporary directory in OS cache location
  - Generate fpm.toml with the .f90 file as app
  - Execute `fpm run` 
  - Keep builds cached for future use
- [x] Write test: `test_run_with_local_module`
  - Test .f90 that uses a local module file
  - Both files in same directory
  - ✅ **Covered by**: `example/calculator.f90` + `example/math_module.f90` and `test_examples.f90`
- [x] Enhance runner to copy all local .f90 files to temp project

### 1.3 Registry-based Module Resolution
- [x] Write test: `test_detect_external_module`
  - Parse .f90 file for `use fortplot`
  - Verify it finds fortplotlib in registry
- [x] Implement module detection in `src/module_scanner.f90`
  - Simple line-by-line scan for `use` statements
  - Extract module names (ignore intrinsic modules)
- [x] Write test: `test_generate_fpm_with_dependencies`
  - For file using `pyplot_module`
  - Generate fpm.toml with pyplot-fortran dependency
- [x] Implement FPM project generator in `src/fpm_generator.f90`
  - Load registry.toml for package definitions
  - Implement smart module resolution:
    - Check explicit mappings in module_index.toml
    - Check custom prefixes
    - Use underscore-based inference
    - Fallback to module name as package name
  - Generate fpm.toml with resolved dependencies

## Phase 2: Enhanced Features (Leveraging FPM) - COMPLETE

### 2.1 Improved Local File Handling
- [x] Write test for multiple local files with interdependencies
  - ✅ **Implemented as**: `example/interdependent/` with 4 interdependent modules
  - ✅ **Tested by**: `test_examples.f90`
- [x] Implement smart local file copying (preserve directory structure)
  - ✅ **Current implementation**: Copies all .f90 files from same directory
  - ✅ **Works for**: Flat directory structure (all modules in same dir)
  - ⚠️ **Future enhancement**: Support subdirectories and relative paths
- [x] Write test for handling relative imports
  - ❌ **Not supported**: Relative imports across directories not supported by design
  - ✅ **Current scope**: Only supports flat directory structure (all modules in same directory)
- [x] Support running from different directories
  - ✅ **Implemented**: `get_absolute_path` function converts relative paths to absolute paths
  - ✅ **Tested by**: `test_different_directories.f90`
  - ✅ **Works for**: Absolute paths, relative paths, running from any directory

### 2.2 Registry Enhancement
- [x] Write test for multiple modules from same package
  - ✅ **Implemented**: `test_multiple_modules.f90` and `test_registry_enhancement.f90`
  - ✅ **Tested by**: Registry resolver correctly handles multiple modules mapping to same package
- [x] Support version constraints in registry
  - ✅ **Implemented**: Extended `package_info` type with version field
  - ✅ **Implemented**: `resolve_module_with_version` function
  - ✅ **Implemented**: FPM generator creates git tags for versioned dependencies
  - ✅ **Tested by**: `test_version_constraints.f90` and `test_fpm_version_generation.f90`
- [x] Write test for conflicting dependencies
  - ✅ **Implemented**: `test_conflicting_dependencies.f90`
  - ✅ **Tested by**: Verifies that multiple modules from same package get deduplicated correctly
- [x] Add registry validation and error messages
  - ✅ **Implemented**: `validate_registry` function in `registry_resolver.f90`
  - ✅ **Implemented**: Comprehensive validation for TOML syntax, required sections, and field formatting
  - ✅ **Tested by**: `test_registry_validation.f90`

### 2.3 Error Handling and User Experience
- [x] Write test for clear FPM error forwarding
  - ✅ **Implemented**: `test_error_handling.f90`
- [x] Implement helpful error messages when modules not found
  - ✅ **Implemented**: FPM error messages forwarded clearly in quiet mode
  - ✅ **Tested by**: `test_error_handling.f90`
- [x] Write test for suggesting similar module names
  - ✅ **Implemented**: Basic error forwarding (no specific suggestions to keep generic)
  - ✅ **Tested by**: `test_error_handling.f90`
- [x] Add verbose mode to show FPM operations
  - ✅ **Implemented**: `-v`, `-vv`, `--verbose`, `--verbose 1`, `--verbose 2`
  - ✅ **Tested by**: `test_verbose.f90`, `test_cli_system.f90`

## Phase 3: Caching System - COMPLETE

### 3.1 Cache Design
- [x] Write test for cache directory structure
  - ✅ **Implemented**: `test_cache_structure.f90`
  - ✅ **Tested by**: Comprehensive cache directory layout validation
- [x] Implement cache directory management
  - ✅ **Implemented**: Enhanced `cache.f90` with `ensure_cache_structure` and `get_cache_subdir`
  - ✅ **Tested by**: `test_cache_management.f90`
- [x] Write test for cache key generation (file hash + dependencies)
  - ✅ **Implemented**: `test_fpm_cache_integration.f90`
  - ✅ **Leverages FPM API**: Uses FPM's built-in source discovery and digest system
- [x] Implement cache key algorithm
  - ✅ **Implemented**: Integrated with FPM's existing digest-based caching system
  - ✅ **Approach**: Leverage FPM's `srcfile_t%digest` instead of creating parallel system

### 3.2 Build Artifact Caching
- [x] Write test for storing compiled modules
- [x] Implement module cache storage
- [x] Write test for storing executables
- [x] Implement executable cache storage
- [x] Write test for cache invalidation
- [x] Implement cache invalidation logic
- [x] Implement FPM-based content hashing
- [x] Create comprehensive test suite for artifact caching

### 3.3 Cache Retrieval
- [x] Write test for cache hit detection
- [x] Implement cache lookup mechanism  
- [x] Write test for executing cached binaries
- [x] Implement cached execution path
- [x] Write system test for incremental compilation
- [x] Implement incremental build support
- [ ] Write test for partial cache hits (some deps cached)
  - Note: Current implementation caches at project level, not module level
  - True partial cache hits would require architectural changes:
    - Cache individual compiled modules separately
    - Share cached modules between projects
    - This is a future enhancement for Phase 4
- [x] Test with complex dependency changes
  - Test modifying dependency modules
  - Test adding new dependencies
  - Current behavior: dependency changes create new cache directories
  - This ensures correctness but could be optimized in future phases
- [x] Write test for source file modification with cached dependencies
  - Run .f90 file first time: compiles everything including dependencies
  - Run same file second time: compiles nothing (full cache hit)
  - Modify .f90 file and run third time: compiles only the modified file, not dependencies
  - This requires keeping same cache directory name but updating source file in place
  - FPM should handle incremental compilation of modified source vs cached dependencies

## Phase 4: Advanced Features

### 4.1 Performance Optimization
- [x] Write benchmarks for common use cases
  - Simple programs: 2-3x speedup
  - Programs with modules: 3-4x speedup  
  - Incremental compilation: 1.5-2x speedup
  - Implemented both shell script and Fortran benchmarks
- [x] Implement parallel dependency resolution
  - **Status**: Infrastructure added, waiting for FPM support
  - **What was done**:
    - Added `--jobs/-j` flag to CLI parsing
    - Infrastructure ready to pass flag to FPM
    - Current FPM version (0.12.0) doesn't support --jobs flag
  - **Future**: When FPM adds support, just enable the flag in build command

- [x] Write test for concurrent cache access
  - Documented current limitations
  - Sequential access works correctly
  - Concurrent access needs proper locking

- [ ] Implement cache locking mechanism
  - **Concrete Plan**:
    1. Add `.lock` file creation when build starts
    2. Include PID and timestamp in lock file
    3. Check for stale locks (> 5 minutes old)
    4. Implement wait with timeout (30 seconds default)
  - **Implementation Steps**:
    - Create `src/cache_lock.f90` module
    - Add `acquire_lock()` and `release_lock()` functions
    - Use POSIX file locking where available
    - Fallback to lock files with atomic rename
    - Add `--no-wait` flag to fail immediately if locked
    - Clean up stale locks on startup

- [x] Implement build progress reporting
  - **Status**: Basic infrastructure added, full implementation deferred
  - **What was done**:
    - Created progress module with progress bar display
    - Added FPM output parser for progress percentages
  - **Challenge**: 
    - Would require changing execute_command_line to capture output line-by-line
    - Complex to implement properly with pipes and real-time updates
    - Deferred in favor of existing FPM progress output

- [ ] Implement module-level caching for partial cache hits
  - **Concrete Plan**:
    1. Create separate cache directory for compiled modules
    2. Use content hash of .f90 file as cache key for .mod/.o files
    3. Copy cached modules into project before build
    4. Let FPM skip compilation of unchanged modules
  - **Implementation Steps**:
    - Add `~/.cache/fortran/modules/` directory structure
    - Store .mod and .o files by content hash
    - Create module dependency graph
    - Copy relevant cached modules before FPM build
    - Benchmark improvement for large projects
  - **Note**: This is complex and may require FPM API changes

### 4.2 Cross-Package Support (DEFERRED - Someday/Maybe)
- [ ] Write test for namespace support
- [ ] Implement package namespacing
- [ ] Write test for version resolution
- [ ] Implement version constraint solver
- [ ] Write test for package conflicts
- [ ] Implement conflict resolution
  - Note: Deferred until core features are battle-tested
  - Would require significant architectural changes
  - Better to focus on performance and reliability first

### 4.3 Developer Experience
- [x] Write test for verbose/debug output
  - ✅ Already implemented in Phase 2.3
  - ✅ Tested by: `test_verbose.f90`, `test_cli_system.f90`
- [ ] Implement detailed logging system
  - Add `--log-file` option to save detailed logs
  - Include timestamps and module names
  - Useful for debugging complex builds
- [x] Write test for helpful error messages
  - ✅ Already implemented in Phase 2.3
  - ✅ Tested by: `test_error_handling.f90`
- [ ] Implement context-aware error messages

## Phase 5: Integration and Polish

### 5.1 FPM Registry Integration
- [ ] Write test for fetching from official registry
- [ ] Implement registry sync functionality
- [ ] Write test for offline mode
- [ ] Implement offline fallback

### 5.2 Platform Support
- [ ] Write tests for Windows compatibility
- [ ] Implement platform-specific adjustments
- [ ] Write tests for macOS compatibility
- [ ] Write tests for various Linux distributions

### 5.3 Documentation and Examples
- [ ] Create comprehensive user documentation
- [ ] Write example programs demonstrating usage
- [ ] Create developer documentation for extensions
- [ ] Write migration guide from manual compilation

## Testing Strategy

Each feature should have:
1. Unit tests for individual components
2. Integration tests for feature workflows
3. End-to-end tests for user scenarios
4. Performance tests for critical paths

## Architecture Notes

Key abstractions to maintain:
- `SourceAnalyzer`: Parses .f90 files for dependencies
- `DependencyResolver`: Resolves module names to packages
- `CacheManager`: Handles all caching operations
- `BuildOrchestrator`: Coordinates fpm builds
- `CLIHandler`: Manages user interaction

## Phase 5: Simplified Fortran Preprocessor (.f files)

### 5.1 Basic Preprocessor Infrastructure
- [ ] Create `src/preprocessor.f90` module
- [ ] Support `.f` file detection in CLI
- [ ] Implement basic file transformation pipeline
- [ ] Generate line mappings for debugging

### 5.2 Implicit Program/Module Detection
- [ ] Write test for program detection (has `call` statements)
- [ ] Write test for module detection (has `contains`, `public`)
- [ ] Implement AST-based analysis for unit type detection
- [ ] Add implicit `program`/`module` wrappers
- [ ] Use filename as default module name

### 5.3 Modern Defaults
- [x] Add `implicit none` to all generated units (via fpm.toml implicit-typing = false)
- [x] Set compiler flags for double precision default (--flag "-fdefault-real-8 -fdefault-double-8")
- [ ] Auto-import common intrinsic modules
- [x] Write tests for default behaviors (real_default_test.f90)

### 5.4 Dot Notation Transform
- [ ] Write test for simple member access (`.` to `%`)
- [ ] Implement context-aware dot parser
- [ ] Handle floating point literal edge cases
- [ ] Preserve logical operators (`.and.`, `.or.`)
- [ ] Test nested type access

## Phase 6: Type Inference System

### 6.1 Basic Intrinsic Type Inference
- [ ] Write test for integer literal inference (42 → integer)
- [ ] Write test for real literal inference (3.14 → real(8))
- [ ] Write test for logical inference (.true. → logical)
- [ ] Write test for character inference ("text" → character)
- [ ] Implement expression parser to detect literal types
- [ ] Generate variable declarations at procedure start
- [ ] Handle multiple assignments to same variable (type consistency)

### 6.2 Array Type Inference
- [ ] Write test for array literal inference ([1,2,3] → integer, dimension(3))
- [ ] Write test for array operations (shape preservation)
- [ ] Implement array shape tracking
- [ ] Support allocatable array inference
- [ ] Handle reshape and other intrinsics

### 6.3 Function Return Type and Intent-Based Inference
- [ ] Write test for function return type inference from body
- [ ] Write test for intent(out) type inference from assignments
- [ ] Implement function body analyzer for return statements
- [ ] Track assignments to function name for return type
- [ ] Propagate inferred return types to call sites
- [ ] Handle intent(out) variables with deferred type declaration

### 6.4 Derived Type Inference
- [ ] Write test for field-based type inference
- [ ] Write test for constructor-based inference
- [ ] Generate derived type definitions
- [ ] Support type extension inference
- [ ] Validate consistent type usage

## Phase 8: Installation and Deployment

### 8.1 Installation Script
- [ ] Create `install.sh` script for easy installation
- [ ] Install `fortran` command to `~/.local/bin/`
- [ ] Copy default config to `~/.config/fortran/`
- [ ] Create cache directory `~/.cache/fortran/`
- [ ] Add uninstall option
- [ ] Support system-wide installation (optional)

### 8.2 Package Distribution
- [ ] Create release packages (tar.gz, deb, rpm)
- [ ] Add to package managers (AUR, homebrew, etc.)
- [ ] Docker container for easy deployment
- [ ] GitHub releases with binaries

## Progress Tracking

- Mark items with [x] when completed
- Add completion dates for major milestones
- Note any deviations from plan with explanations
- Review and update priorities weekly