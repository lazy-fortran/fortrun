# Fortran Standards Evolution

This directory contains documentation for various Fortran standards and their evolution from Fortran 95 to the present, including our proposed Lazy Fortran (lazy 2507) superset.

## Standards Timeline

### Fortran 95 (1995) - Foundation
The baseline modern Fortran standard that introduced:
- **FORALL** construct and statement
- **PURE** and **ELEMENTAL** procedures
- **Pointer initialization** and structure default initialization
- **WHERE** construct extensions
- **CPU_TIME** intrinsic subroutine
- Removed obsolescent features from FORTRAN 77

### Fortran 2003 (2003) - Object-Oriented Revolution
Major paradigm shift introducing object-oriented programming:
- **Object-oriented programming**: type extension, polymorphism, type-bound procedures
- **Parameterized derived types** (PDTs)
- **Abstract interfaces** and type-bound procedure pointers
- **ASSOCIATE** construct for aliasing
- **Interoperability with C** via ISO_C_BINDING
- **IEEE arithmetic support** modules
- **Stream I/O** for binary file access
- **Command line argument** access (GET_COMMAND_ARGUMENT)
- **Environmental variables** access (GET_ENVIRONMENT_VARIABLE)
- **ALLOCATABLE** improvements: scalars, character lengths, function results

### Fortran 2008 (2008) - Parallel Computing
Focus on parallel programming and modern computing:
- **Coarrays** for parallel programming (SPMD model)
- **DO CONCURRENT** construct for parallelizable loops
- **Submodules** for better code organization
- **CONTIGUOUS** attribute for array optimization
- **BLOCK** construct for local scope
- **ERROR STOP** statement
- **NEWUNIT** specifier in OPEN
- **G0** format descriptor
- **Complex intrinsics**: ATAN2, LOG with complex arguments
- **Execute command line** (EXECUTE_COMMAND_LINE)

### Fortran 2018 (2018) - Enhanced Parallelism
Refinements and enhanced parallel features:
- **Enhanced coarray features**: teams, events, and collective subroutines
- **IMPORT** enhancements in interfaces
- **Implied-shape arrays** with PARAMETER
- **SELECT RANK** construct for assumed-rank arrays
- **Enhanced C interoperability**: assumed-type, assumed-rank, C descriptors
- **IEEE arithmetic enhancements**
- **Recursive I/O** allowed in more contexts
- **DEFAULT(NONE)** in OpenMP/OpenACC regions
- **RANDOM_INIT** intrinsic for reproducible random numbers

### Fortran 2023 (2023) - Modern Conveniences
Latest standard with quality-of-life improvements:
- **Conditional expressions**: ternary operator-like functionality
- **TYPEOF** and **CLASSOF** for type introspection
- **Enumeration types** (ENUM)
- **BITS** strings for bit manipulation
- **AT** format descriptor for formatted I/O
- **Longer lines**: 10,000 character maximum
- **Multiple subscripts** in array sections
- **Half-precision reals** (16-bit)
- **Enhanced generic programming** features

### lazy 2507 (Proposed) - Lazy Fortran
Our proposed superset adding Python-like conveniences:
- **Strict superset**: Any valid Fortran 95/2003/2008/2018/2023 program works unchanged
- **Type inference**: Automatic variable typing from assignments
- **Implicit program**: No boilerplate for simple scripts
- **Modern defaults**: implicit none, real(8), intent(in)
- **Future features**: f-strings, list comprehensions, pattern matching
- **Zero configuration**: Automatic module resolution and building

## Key Evolution Themes

1. **Performance → Productivity**: Early standards focused on computational efficiency; modern standards emphasize developer productivity

2. **Procedural → Multi-Paradigm**: Evolution from purely procedural (F77) to supporting OOP (F2003), parallel (F2008/F2018), and functional programming styles

3. **Isolated → Interoperable**: From Fortran-only to seamless C interoperability and system integration

4. **Explicit → Inferrable**: Movement toward optional type inference and automatic behaviors while maintaining explicit control when needed

5. **Monolithic → Modular**: From program units to modules, submodules, and better code organization features

## Compatibility Notes

- Each standard maintains backward compatibility with previous versions
- Obsolescent features are deprecated but not removed
- Deleted features (from F77) were given long deprecation periods
- lazy 2507 (Lazy Fortran) maintains this tradition as a **strict superset** - all standard Fortran programs remain valid

## File Extensions Convention

- `.f90` - Free-form Fortran (any standard)
- `.f03` - Fortran 2003 specific features
- `.f08` - Fortran 2008 specific features  
- `.f18` - Fortran 2018 specific features
- `.f` - Fixed-form (legacy) or Lazy Fortran mode

## Implementation Support

Most modern compilers support:
- **Full**: Fortran 95, 2003, 2008
- **Partial**: Fortran 2018, 2023
- **Experimental**: lazy 2507 (via our preprocessor/compiler frontend)