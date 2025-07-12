# Implementation Plan

## Compiler Flag Changes

### Current Behavior
The tool currently applies opinionated compiler flags to all Fortran files:
- `implicit-typing = false` in fmp.toml (enforces `implicit none`)
- `--flag "-fdefault-real-8 -fdefault-double-8"` (double precision defaults)

### Required Changes

#### 1. Conditional Compiler Flags
**Problem**: Opinionated flags should only apply to preprocessed `.f` files, not standard `.f90` files.

**Solution**: 
- **Standard `.f90` files**: Use default FPM behavior with no custom compiler flags
- **Preprocessed `.f` files**: Apply opinionated flags:
  - `implicit-typing = false` 
  - `--flag "-fdefault-real-8 -fdefault-double-8"`

#### 2. User-Defined Flags
**New Feature**: Add `--flag` option to CLI

**Implementation**:
- CLI option: `--flag "custom-flags"` 
- Behavior: Pass the provided flags directly to FPM's `--flag` argument
- Precedence: User flags should override default flags
- Scope: Apply to both `.f90` and preprocessed `.f` files when specified

#### 3. Flag Application Logic
```
IF file_extension == ".f90" AND user_flags_provided:
    apply_flags(user_flags)
ELIF file_extension == ".f90":
    apply_no_flags()  # Default FPM behavior
ELIF file_extension == ".f" AND user_flags_provided:
    apply_flags(opinionated_flags + user_flags)
ELIF file_extension == ".f":
    apply_flags(opinionated_flags)
```

### Implementation Areas

1. **CLI Module** (`src/cli.f90`):
   - Add `--flag` option parsing
   - Store user-provided flags

2. **FPM Generator** (`src/fpm_generator.f90`):
   - Modify flag generation logic based on file type
   - Combine opinionated and user flags appropriately

3. **Runner Module** (`src/runner.f90`):
   - Pass flag information to FPM generator
   - Coordinate between file type detection and flag application

### Testing Requirements

1. **Unit Tests**:
   - Test flag generation for `.f90` vs `.f` files
   - Test user flag parsing and combination
   - Test precedence rules

2. **Integration Tests**:
   - Verify `.f90` files compile with default flags
   - Verify `.f` files compile with opinionated flags
   - Verify `--flag` option works for both file types
   - **Multiple `.f` files**: Test program + module where module uses filename as default module name before preprocessing

3. **System Tests**:
   - End-to-end CLI testing with various flag combinations
   - Backward compatibility testing
   - Multi-file `.f` dependency resolution (program calling module functions)

### Migration Notes

- **Breaking Change**: `.f90` files will no longer have opinionated flags applied by default
- **Benefit**: More predictable behavior for standard Fortran users
- **Compatibility**: Existing `.f` file behavior remains unchanged