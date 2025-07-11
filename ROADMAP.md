# ROADMAP.md

## Project Vision

The `fortran` CLI tool aims to make Fortran development as seamless as Python, with automatic dependency resolution, transparent compilation, and eventually a simplified syntax that compiles to standard Fortran.

## Current Status (Phase 1 Complete)

### ✅ Implemented Features
- **Basic CLI**: `fortran <file.f90>` executes Fortran programs transparently
- **OS Cache Management**: Projects cached in standard OS locations (`~/.cache/fortran` on Linux)
- **Local Dependencies**: Automatically includes local `.f90` modules from the same directory
- **FPM Integration**: Leverages FPM for building and dependency management
- **Module Registry**: TOML-based registry with smart module resolution (prefix and underscore rules)
- **Modern Defaults**:
  - `implicit none` enforced by default (via fpm.toml `implicit-typing = false`)
  - Double precision (`real*8`) as default for `real` type
  - Compiler flags automatically applied: `-fdefault-real-8 -fdefault-double-8`

## Near-term Roadmap

### Phase 2: Registry-based Dependencies (In Progress)
- [ ] Module scanner to detect `use` statements
- [ ] Automatic package resolution from registry
- [ ] Dynamic FPM project generation with dependencies
- [ ] Support for version constraints

### Phase 3: Smart Caching
- [ ] Content-based hashing for cache invalidation
- [ ] Reuse previous builds when files haven't changed
- [ ] Leverage FPM's internal hashing mechanisms
- [ ] Parallel build support

### Phase 4: Enhanced Registry
- [ ] Sync with official FPM registry
- [ ] Support for git tags and branches
- [ ] Local package overrides
- [ ] Namespace support (e.g., `fortran-lang/stdlib`)

## Long-term Vision: Simplified Fortran (.f files)

### Phase 5: Basic Preprocessor
Implement a preprocessor that transforms `.f` files (simplified syntax) into standard `.f90` files.

#### 5.1 Implicit Program/Module Units
- **Feature**: Write programs without explicit `program` wrapper
- **Feature**: Write modules without explicit `module` wrapper
- **Implementation**:
  ```fortran
  ! example.f (simplified)
  print *, "Hello World"
  
  ! Transforms to example.f90:
  program example
    implicit none
    print *, "Hello World"
  end program example
  ```
- **Module Detection**: Analyze for `contains`, `public`, `private` keywords
- **Program Detection**: Look for `call`, main execution statements
- **Default**: Module name = filename (without extension)

#### 5.2 Modern Defaults ✅ (Partially Complete)
- **Feature**: `implicit none` by default (no need to write it) ✅
  - **Implemented**: Using `implicit-typing = false` in fpm.toml
- **Feature**: `real(kind=8)` as default real type ✅
  - **Implemented**: Using `--flag "-fdefault-real-8 -fdefault-double-8"`
- **Feature**: Automatic `use, intrinsic :: iso_fortran_env`
  - **Status**: Not yet implemented

### Phase 6: Syntax Enhancements

#### 6.1 Dot Notation for Type Members
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

#### 6.2 Enhanced Import System
- **Feature**: Python-like imports
  ```fortran
  ! Simplified syntax
  from stdlib use string_type, to_string
  import numpy as np
  
  ! Transforms to:
  use stdlib_string_type, only: string_type, to_string
  use numpy_module, np => numpy
  ```

### Phase 7: Advanced Preprocessor Features

#### 7.1 Type Inference
- Optional type declarations for obvious cases
- Automatic interface generation
- Smart generic resolution

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

The goal is to make Fortran as approachable as Python while maintaining its performance advantages and numerical computing strengths.