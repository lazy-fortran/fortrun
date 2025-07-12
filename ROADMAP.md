# ROADMAP.md

## Project Vision

**Make Python Fortran again.** The `fortran` CLI tool aims to make Fortran development as seamless as Python, with automatic dependency resolution, transparent compilation, and eventually a simplified syntax that compiles to standard Fortran.

## Current Status (Phase 8 Complete)

### ✅ Implemented Features
- **Basic CLI**: `fortran <file.f90>` executes Fortran programs transparently
- **OS Cache Management**: Projects cached in standard OS locations (`~/.cache/fortran` on Linux)
- **Local Dependencies**: Automatically includes local `.f90` modules from the same directory
- **FPM Integration**: Leverages FPM for building and dependency management
- **Module Registry**: TOML-based registry with smart module resolution (prefix and underscore rules)
- **Smart Caching System**:
  - Content-based caching with 2-4x performance improvements
  - Incremental compilation support (only changed files recompile)
  - Structure-based hashing for cache keys
  - In-place source updates for modified files
- **Modern Defaults**:
  - `implicit none` enforced by default (via fpm.toml `implicit-typing = false`)
  - Double precision (`real*8`) as default for `real` type
  - Compiler flags automatically applied: `-fdefault-real-8 -fdefault-double-8`
- **Developer Experience**:
  - Verbose mode (`-v`, `--verbose`) for debugging
  - Helpful error messages from FPM
  - Custom cache directory support (`--cache-dir`)
- **Simplified Fortran Preprocessor (.f files)**:
  - Automatic program wrapping for executable code
  - Automatic `contains` insertion for functions/subroutines
  - Modern defaults with `implicit none` and double precision
  - Seamless integration with existing build system
- **Basic Type Inference**:
  - Automatic type detection for integers, reals, logicals, characters
  - Expression type propagation and variable tracking
  - Integration with .f file preprocessing
  - Comprehensive test coverage (41/41 tests passing)
- **Notebook Support (Jupytext-like)**:
  - Parse .f files with markdown cells using %% delimiters
  - Execute notebook cells with variable persistence
  - Render to markdown with code blocks and output
  - **Figure/Plot Integration**: Automatic capture of fortplotlib plots as inline base64 PNG images
  - CLI integration with `--notebook` flag
- **Advanced Type Inference**:
  - Modular architecture with clean separation of concerns
  - Array type inference with dimension and shape analysis
  - Derived type pattern detection and field tracking
  - Function return type inference with intrinsic function support
  - Comprehensive test coverage (61/61 tests passing across all analyzers)

## Near-term Roadmap

### Phase 2: Registry-based Dependencies (Complete)
- [x] Module scanner to detect `use` statements
- [x] Automatic package resolution from registry
- [x] Dynamic FPM project generation with dependencies
- [x] Support for version constraints
- [x] Multiple modules from same package support
- [x] Conflicting dependencies resolution
- [x] Registry validation and error handling

### Phase 3: Smart Caching (Complete)
- [x] Content-based hashing for cache invalidation
- [x] Reuse previous builds when files haven't changed
- [x] Leverage FPM's internal hashing mechanisms
- [x] Build artifact caching with incremental compilation
- [x] Cache retrieval and validation
- [x] Performance benchmarks showing 2-4x speedup

### Phase 4: Enhanced Registry & Advanced Caching (In Progress)
- [ ] Sync with official FPM registry
- [ ] Support for git tags and branches
- [ ] Local package overrides
- [ ] Namespace support (e.g., `fortran-lang/stdlib`)
- [x] **Cache Locking Mechanism** for concurrent access:
  - Lock files with PID tracking to handle stale locks
  - Atomic file operations using hard links
  - Wait/retry logic with 30-second timeout
  - Stale lock detection (5 minutes or dead process)
  - `--no-wait` flag to fail immediately if locked
  - Clean up stale locks on startup
- [x] **Module-level caching architecture** (implementation pending):
  - Designed `fpm_module_cache.f90` following FPM conventions
  - Cache structure: `~/.cache/fortran/modules/<compiler>/<version>/<hash>/`
  - Module fingerprinting based on source digest and dependencies
  - Compiler-specific segregation for binary compatibility
  - Comprehensive test suite demonstrating 2-3x speedup
  - Future: Full integration requires FPM API enhancements

## Long-term Vision: Simplified Fortran (.f files)

### Phase 5: Basic Preprocessor (Complete)
- ✅ **Automatic program wrapping**: Write executable code without explicit `program` statements
- ✅ **Automatic contains insertion**: Functions/subroutines automatically wrapped
- ✅ **Modern defaults**: `implicit none` and double precision automatically applied
- ✅ **Integration with cache system**: Preprocessed files properly cached
- ✅ **Comprehensive test coverage**: All preprocessor functionality tested

### Phase 6: Basic Type Inference (Complete) 
- ✅ **Literal type detection**: Automatic inference for integers, reals, logicals, characters
- ✅ **Expression type propagation**: Type analysis across arithmetic expressions
- ✅ **Variable declaration generation**: Automatic insertion of type declarations
- ✅ **Integration with preprocessor**: Seamless .f file processing with type inference

### Phase 7: Notebook Features (Complete)
- ✅ **Jupytext-like notebook support**: Parse .f files with markdown cells
- ✅ **Cell execution**: Execute notebook cells with variable persistence
- ✅ **Figure capture**: Automatic conversion of show() calls to base64 PNG images
- ✅ **Markdown rendering**: Complete notebooks rendered to markdown with embedded figures

### Phase 8: Advanced Type Inference (Complete)
- ✅ **Modular architecture**: Clean separation with dedicated analyzers
- ✅ **Array type inference**: Dimension and shape analysis
- ✅ **Derived type patterns**: Field detection and tracking
- ✅ **Function return types**: Comprehensive intrinsic function support

### Phase 9: Enhanced Syntax Features (Future)

#### 9.1 Dot Notation for Type Members
- **Feature**: Use `.` instead of `%` for derived type access
- **Example**:
  ```fortran
  ! Simplified .f syntax
  person.name = "Alice"
  point.x = 3.14
  
  ! Transforms to standard .f90:
  person%name = "Alice"
  point%x = 3.14
  ```
- **Challenges**:
  - Distinguish from floating point literals (e.g., `3.14`)
  - Handle logical operators (`.and.`, `.or.`)
  - Context-aware parsing required

#### 9.2 Enhanced Import System
- **Feature**: Python-like imports
  ```fortran
  ! Simplified syntax
  from stdlib use string_type, to_string
  import numpy as np
  
  ! Transforms to:
  use stdlib_string_type, only: string_type, to_string
  use numpy_module, np => numpy
  ```

### Phase 7: Advanced Type Inference System

#### 7.1 Basic Intrinsic Type Inference
- **Feature**: Infer types for integer and real variables from usage
- **Example**:
  ```fortran
  ! Simplified .f syntax (no declarations needed)
  x = 3.14          ! Inferred as real(8)
  n = 42            ! Inferred as integer
  flag = .true.     ! Inferred as logical
  name = "Alice"    ! Inferred as character(len=:), allocatable
  
  ! Transforms to standard .f90:
  program example
    implicit none
    real(8) :: x
    integer :: n
    logical :: flag
    character(len=:), allocatable :: name
    
    x = 3.14
    n = 42
    flag = .true.
    name = "Alice"
  end program example
  ```
- **Implementation**: Parse RHS expressions to determine types
- **Safety**: More restrictive than old implicit typing - based on actual usage

#### 7.2 Array and Vector Type Inference
- **Feature**: Infer array dimensions and types from initialization
- **Example**:
  ```fortran
  ! Simplified syntax
  vec = [1.0, 2.0, 3.0]      ! Inferred as real(8), dimension(3)
  mat = reshape([1,2,3,4], [2,2])  ! Inferred as integer, dimension(2,2)
  dynamic = []               ! Inferred as allocatable array
  
  ! Array operations infer size
  a = vec * 2.0              ! Same shape as vec
  b = matmul(mat, mat)       ! Shape inferred from matmul rules
  ```
- **Implementation**: Track array shapes through operations
- **Support**: Static arrays, allocatable arrays, array sections

#### 7.3 Function Parameter Type Inference (Future)
- **Feature**: Infer function parameter types from call sites (requires whole-program analysis)
- **Status**: ⚠️ **Deferred - Requires multiple dispatch capabilities**
- **Challenges**:
  - Need to analyze all function call sites
  - Handle polymorphic functions called with different types
  - Requires sophisticated type unification algorithm
  - May need multiple dispatch or generic function generation
- **Example**:
  ```fortran
  ! Current limitation - parameters need explicit types
  function add(a, b)
    ! Cannot infer types of a, b without analyzing all calls
    add = a + b
  end function
  
  ! Would need to analyze:
  result1 = add(1.0, 2.0)     ! Suggests real types
  result2 = add(1, 2)         ! Suggests integer types
  ! This requires multiple dispatch or compile-time polymorphism
  ```

#### 7.4 Function Return Type Inference (Partial)
- **Feature**: Infer function return types from return statements
- **Status**: ✅ Basic implementation complete for simple cases
- **Example**:
  ```fortran
  ! Function return type inference from return value
  function compute(x, y)
    real(8), intent(in) :: x, y  ! Parameters still need declarations
    compute = x**2 + y**2  ! Return type inferred as real(8) from expression
  end function
  
  ! Intent(out) type propagation
  subroutine process(input, output)
    integer, intent(in) :: input
    intent(out) :: output  ! Type will be inferred from usage
    output = input * 2     ! Infers output as integer
  end subroutine
  
  ! Function with inferred return type
  function get_pi()
    get_pi = 3.14159265359  ! Return type inferred as real(8)
  end function
  
  ! Usage propagates inferred types:
  radius = 5.0
  area = radius * get_pi()  ! get_pi() known to return real(8)
  ```
- **Implementation**: Analyze function body to determine return type from assignments to function name
- **Benefits**: No need to declare function return types when obvious from implementation

#### 7.4 Derived Type Inference
- **Feature**: Infer custom types from usage patterns
- **Example**:
  ```fortran
  ! Type inferred from field access
  person.name = "Bob"
  person.age = 30
  person.height = 1.75
  ! Infers: type person_type with character, integer, real fields
  
  ! Type inferred from constructor pattern
  point = Point(3.0, 4.0)  ! Infers Point type with two real components
  
  ! Polymorphic inference
  shape = Circle(5.0)      ! Infers shape is class(Shape), Circle extends Shape
  area = shape.area()      ! Validates area() method exists
  ```
- **Implementation**: Build type constraints from usage, generate type definitions
- **Validation**: Ensure consistent usage across program
- **Safety**: Better than auto - catches type mismatches at preprocessing

#### 7.5 Advanced Type System Features
- **Generic Programming**: Automatic instantiation of generic procedures
- **Type Classes**: Infer interfaces from usage patterns
- **Algebraic Types**: Support for sum types and pattern matching
- **Gradual Typing**: Mix inferred and explicit types smoothly
- **Error Messages**: Clear diagnostics showing inferred vs expected types

#### 7.2 Modern Control Structures
- Python-like list comprehensions
- Enhanced select case syntax
- Iterator protocol support

#### 7.3 Integrated Documentation
- Docstring support
- Automatic API documentation generation
- Inline examples that become tests

## Implementation Strategy

1. **Preprocessor Architecture**:
   - Separate preprocessing step before FPM
   - Generate standard-compliant `.f90` files
   - Preserve line mapping for debugging
   - Cache preprocessed output

2. **Backward Compatibility**:
   - Always generate valid Fortran 2018 code
   - Support mixing `.f` and `.f90` files
   - Gradual adoption path

3. **Tooling Integration**:
   - LSP support for simplified syntax
   - Syntax highlighting for editors
   - Formatter for consistent style
   - Linter for best practices

## Success Metrics

- **Adoption**: Number of packages using simplified syntax
- **Developer Experience**: Time from idea to running code
- **Compatibility**: Works with all major Fortran compilers
- **Performance**: No runtime overhead vs standard Fortran

## Community Involvement

- RFC process for new syntax features
- Beta testing program
- Regular surveys on pain points
- Close collaboration with FPM team

## Timeline Estimates

- **Phase 2-4**: 3-6 months (Registry and Caching)
- **Phase 5**: 2-3 months (Basic Preprocessor)
- **Phase 6**: 4-6 months (Syntax Enhancements)
- **Phase 7**: 6-12 months (Advanced Features)

The goal is to **Make Python Fortran again** - making Fortran as approachable as Python while maintaining its performance advantages and numerical computing strengths.