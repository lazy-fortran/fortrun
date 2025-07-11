# TODO.md

This document tracks the development tasks for the `fortran` CLI tool. It should be updated as items are completed or plans change.

## Development Principles
- Follow TDD (Test-Driven Development) - write tests first
- Apply SOLID principles throughout
- Keep work units small, manageable, and self-contained
- Each task should be completable independently
- Prefer using the existing fpm fork (https://github.com/krystophny/fpm) minimally or not at all

## Phase 1: Foundation and Basic CLI

### 1.1 Project Setup
- [ ] Set up basic CLI structure in `app/main.f90`
  - Parse command line arguments using Fortran intrinsics
  - Support: `fortran <file.f90>` and `fortran --help`
  - Exit with proper codes (0 for success, 1 for errors)
- [ ] Create test framework structure
  - Set up `test/test_cli.f90` for CLI testing
  - Create helper module `test/test_utils.f90` for assertions
  - Add test runner in fpm.toml
- [ ] Define core interfaces in `src/`
  - `src/cli_parser.f90`: Command line parsing module
  - `src/runner.f90`: Main execution orchestrator
  - `src/errors.f90`: Error types and handling

### 1.2 Minimal Viable Product
- [ ] Write test: `test_run_simple_hello_world`
  - Create test .f90 file that prints "Hello, World!"
  - Verify it compiles and runs via our tool
  - Check output matches expected
- [ ] Implement basic execution in `src/runner.f90`
  - Create temporary directory in `/tmp/fortran_build_XXXXX`
  - Copy input .f90 to `app/` in temp directory
  - Generate minimal fpm.toml
  - Execute `fpm run` and capture output
  - Clean up temp directory
- [ ] Write test: `test_invalid_file_error`
  - Test with non-existent file
  - Test with non-.f90 file
  - Test with invalid Fortran syntax
- [ ] Implement error handling in `src/errors.f90`
  - FileNotFoundError
  - InvalidFileTypeError
  - CompilationError with fpm output
  - User-friendly error messages

### 1.3 Module Detection
- [ ] Write test: `test_detect_single_use_statement`
  - Parse: `use my_module`
  - Parse: `use my_module, only: some_func`
  - Parse: `use, intrinsic :: iso_fortran_env`
- [ ] Implement basic parser in `src/source_parser.f90`
  - Read .f90 file line by line
  - Regex pattern for `use` statements
  - Extract module names
  - Handle Fortran comments and continuations
- [ ] Write test: `test_detect_multiple_modules`
  - File with multiple use statements
  - Nested module dependencies
  - Ignore intrinsic modules
- [ ] Implement dependency graph in `src/dependency_graph.f90`
  - Module type with name and source file
  - Graph structure with add_dependency method
  - Topological sort for build order
  - Cycle detection

## Phase 2: Dependency Resolution

### 2.1 Local Module Resolution
- [ ] Write test for finding modules in current directory
- [ ] Implement local module search functionality
- [ ] Write test for recursive dependency resolution
- [ ] Implement recursive dependency resolver

### 2.2 Registry Integration
- [ ] Write test for loading registry.toml
- [ ] Implement TOML registry parser
- [ ] Write test for module-to-package lookup
- [ ] Implement module index generator
- [ ] Write test for resolving external packages
- [ ] Implement package fetching using fpm

### 2.3 Build System Integration
- [ ] Write test for creating temporary fpm project structure
- [ ] Implement dynamic fpm.toml generation
- [ ] Write test for building with dependencies
- [ ] Implement fpm build invocation
- [ ] Write test for build error handling
- [ ] Implement build error reporting

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