# REVIEW.md

This document provides review criteria for the `fortran` CLI tool based on the principles described in `CLAUDE.md` and `README.md`.

## Review Principles

### 1. **Test-Driven Development (TDD)**
- [ ] All new features have comprehensive test coverage
- [ ] Unit tests for individual components
- [ ] Integration tests for feature workflows
- [ ] End-to-end tests for user scenarios
- [ ] Performance tests for critical paths
- [ ] Tests written before implementation (where applicable)

### 2. **SOLID Principles**
- [ ] **Single Responsibility**: Each module has one clear purpose
- [ ] **Open/Closed**: Extensions don't require modifying existing code
- [ ] **Liskov Substitution**: Interfaces are properly abstracted
- [ ] **Interface Segregation**: Modules only depend on what they need
- [ ] **Dependency Inversion**: High-level modules don't depend on low-level details

### 3. **Opinionated Design for Good**
- [ ] Modern Fortran practices enforced by default
- [ ] `implicit none` applied automatically
- [ ] Double precision (`real(8)`) as default
- [ ] Free-form source format used
- [ ] Zero configuration required for basic usage
- [ ] Sensible defaults that promote best practices

### 4. **Developer Experience Excellence**
- [ ] **Instant execution**: `fortran file.f90` works like `python file.py`
- [ ] **Clear error messages**: Helpful, actionable error reporting
- [ ] **Comprehensive CLI**: All expected flags and options
- [ ] **Backward compatibility**: Works with existing Fortran code
- [ ] **Gradual adoption**: Can be integrated into existing workflows

### 5. **Architecture Quality**
- [ ] **Key abstractions maintained**:
  - SourceAnalyzer: Parses files for dependencies
  - DependencyResolver: Resolves module names to packages
  - CacheManager: Handles all caching operations
  - BuildOrchestrator: Coordinates FPM builds
  - CLIHandler: Manages user interaction
- [ ] **Minimal FPM fork usage**: Leverage standard FPM when possible
- [ ] **Modular design**: Clear separation of concerns

## Feature-Specific Review Criteria

### Preprocessor (.f files)
- [ ] **File detection**: Correctly identifies .f and .F files
- [ ] **Program wrapping**: Automatically adds `program main` when needed
- [ ] **Contains insertion**: Adds `contains` before functions/subroutines
- [ ] **Indentation preservation**: Maintains proper code formatting
- [ ] **Edge case handling**: Empty files, comments, existing programs
- [ ] **Error handling**: Clear messages for preprocessing failures

### Type Inference System
- [ ] **Literal detection**: Correctly identifies integer, real, logical, character literals
- [ ] **Expression evaluation**: Handles arithmetic expressions with type promotion
- [ ] **Intrinsic functions**: Recognizes common intrinsic return types
- [ ] **Variable tracking**: Maintains type environment across assignments
- [ ] **Declaration generation**: Produces correct Fortran declarations
- [ ] **Integration**: Works seamlessly with preprocessor
- [ ] **Conflict resolution**: Handles type conflicts gracefully

### Cache System
- [ ] **Directory structure**: Proper OS-specific cache organization
- [ ] **Content hashing**: Uses FPM's digest system for cache keys
- [ ] **Cache invalidation**: Detects when rebuilds are needed
- [ ] **Parallel safety**: Handles concurrent access with locking
- [ ] **Performance**: Significant speedup for repeated builds

### Registry System
- [ ] **Module resolution**: Maps module names to packages correctly
- [ ] **Version constraints**: Handles git tags and version specifications
- [ ] **Conflict detection**: Identifies and resolves dependency conflicts
- [ ] **Registry validation**: Validates TOML syntax and required fields
- [ ] **Extensibility**: Easy to add new packages and registries

## Code Quality Standards

### Documentation
- [ ] **Comprehensive README**: Clear examples and installation instructions
- [ ] **Inline comments**: Complex logic explained
- [ ] **Example programs**: Working examples for each feature
- [ ] **Error messages**: Self-documenting error conditions

### Performance
- [ ] **Build speed**: Significantly faster than manual compilation
- [ ] **Memory usage**: Reasonable memory footprint
- [ ] **Cache efficiency**: Effective caching reduces rebuild times
- [ ] **Parallel builds**: Leverages FPM's parallel compilation

### Maintainability
- [ ] **Small changes**: Features implemented incrementally
- [ ] **Clear commits**: Descriptive commit messages
- [ ] **Consistent style**: Follows Fortran coding conventions
- [ ] **Dependency management**: Minimal external dependencies

## Security Considerations
- [ ] **Input validation**: Sanitizes file paths and user input
- [ ] **Temporary files**: Secure handling of cache and temp directories
- [ ] **Git URLs**: Validates git repository URLs
- [ ] **File permissions**: Respects OS file permission models

## Compatibility Requirements
- [ ] **Fortran standards**: Generates standard Fortran 2018 code
- [ ] **Compiler support**: Works with major Fortran compilers
- [ ] **OS support**: Linux, macOS, Windows compatibility
- [ ] **FPM compatibility**: Works with existing FPM projects

## Review Checklist

### Before Merge
- [ ] All tests pass (`fpm test`)
- [ ] Code compiles without warnings
- [ ] Documentation updated for new features
- [ ] Examples work as documented
- [ ] No regression in existing functionality
- [ ] Performance benchmarks (if applicable)

### Post-Merge
- [ ] Integration tests in clean environment
- [ ] User acceptance testing
- [ ] Performance monitoring
- [ ] Documentation accuracy verification

## Success Metrics

### Quantitative
- [ ] **Test coverage**: >95% for new features
- [ ] **Performance**: 2-3x faster than manual builds
- [ ] **Cache hit rate**: >80% for repeated builds
- [ ] **Error rate**: <5% for valid input files

### Qualitative
- [ ] **User experience**: Intuitive and helpful
- [ ] **Error messages**: Clear and actionable
- [ ] **Documentation**: Complete and accurate
- [ ] **Code quality**: Maintainable and extensible

## Review Process

1. **Automated checks**: CI/CD pipeline validates basic requirements
2. **Code review**: Manual review against these criteria
3. **Testing**: Comprehensive testing in multiple environments
4. **Documentation**: Verify all documentation is current
5. **Integration**: Test with existing FPM projects
6. **User testing**: Validate real-world usage scenarios

## Notes

- This is a living document that evolves with the project
- Criteria may be adjusted based on project maturity and requirements
- Focus on delivering value while maintaining quality standards
- Balance perfectionism with practical progress