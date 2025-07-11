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
- [ ] Write test: `test_run_simple_program`
  - Test .f90 file without external dependencies
  - Verify execution via our tool
- [ ] Implement basic execution in `src/runner.f90`
  - Create temporary directory
  - Generate fpm.toml with the .f90 file as app
  - Execute `fpm run` 
  - Clean up
- [ ] Write test: `test_run_with_local_module`
  - Test .f90 that uses a local module file
  - Both files in same directory
- [ ] Enhance runner to copy all local .f90 files to temp project

### 1.3 Registry-based Module Resolution
- [ ] Write test: `test_detect_external_module`
  - Parse .f90 file for `use fortplot`
  - Verify it finds fortplotlib in registry
- [ ] Implement module detection in `src/module_scanner.f90`
  - Simple line-by-line scan for `use` statements
  - Extract module names (ignore intrinsic modules)
- [ ] Write test: `test_generate_fpm_with_dependencies`
  - For file using `pyplot_module`
  - Generate fpm.toml with pyplot-fortran dependency
- [ ] Implement FPM project generator in `src/fpm_generator.f90`
  - Load registry.toml for package definitions
  - Implement smart module resolution:
    - Check explicit mappings in module_index.toml
    - Check custom prefixes
    - Use underscore-based inference
    - Fallback to module name as package name
  - Generate fpm.toml with resolved dependencies

## Phase 2: Enhanced Features (Leveraging FPM)

### 2.1 Improved Local File Handling
- [ ] Write test for multiple local files with interdependencies
- [ ] Implement smart local file copying (preserve directory structure)
- [ ] Write test for handling relative imports
- [ ] Support running from different directories

### 2.2 Registry Enhancement
- [ ] Write test for multiple modules from same package
- [ ] Support version constraints in registry
- [ ] Write test for conflicting dependencies
- [ ] Add registry validation and error messages

### 2.3 Error Handling and User Experience
- [ ] Write test for clear FPM error forwarding
- [ ] Implement helpful error messages when modules not found
- [ ] Write test for suggesting similar module names
- [ ] Add verbose mode to show FPM operations

## Phase 3: Caching System

### 3.1 Cache Design
- [ ] Write test for cache directory structure
- [ ] Implement cache directory management
- [ ] Write test for cache key generation (file hash + dependencies)
- [ ] Implement cache key algorithm

### 3.2 Build Artifact Caching
- [ ] Write test for storing compiled modules
- [ ] Implement module cache storage
- [ ] Write test for storing executables
- [ ] Implement executable cache storage
- [ ] Write test for cache invalidation
- [ ] Implement cache invalidation logic

### 3.3 Cache Retrieval
- [ ] Write test for cache hit detection
- [ ] Implement cache lookup mechanism
- [ ] Write test for executing cached binaries
- [ ] Implement cached execution path
- [ ] Write test for partial cache hits (some deps cached)
- [ ] Implement incremental build support

## Phase 4: Advanced Features

### 4.1 Performance Optimization
- [ ] Write benchmarks for common use cases
- [ ] Implement parallel dependency resolution
- [ ] Write test for concurrent cache access
- [ ] Implement cache locking mechanism

### 4.2 Cross-Package Support
- [ ] Write test for namespace support
- [ ] Implement package namespacing
- [ ] Write test for version resolution
- [ ] Implement version constraint solver
- [ ] Write test for package conflicts
- [ ] Implement conflict resolution

### 4.3 Developer Experience
- [ ] Write test for verbose/debug output
- [ ] Implement detailed logging system
- [ ] Write test for progress indicators
- [ ] Implement build progress reporting
- [ ] Write test for helpful error messages
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

## Progress Tracking

- Mark items with [x] when completed
- Add completion dates for major milestones
- Note any deviations from plan with explanations
- Review and update priorities weekly