# Advanced Type Inference Examples

This directory contains examples demonstrating the advanced type inference capabilities of the Fortran CLI tool.

## Array Type Inference

The `arrays.f` example shows:
- Simple array literals: `[1, 2, 3]` → `integer, dimension(3)`
- Real arrays: `[1.0, 2.0, 3.0]` → `real(8), dimension(3)`  
- Mixed type arrays: `[1, 2.0, 3]` → `real(8), dimension(3)` (promoted)
- Multi-dimensional arrays via `reshape`

## Derived Type Inference

The `derived_types.f` example shows:
- Field access patterns: `person.name = "Alice"` → creates `person_type`
- Multiple field assignments automatically infer the derived type structure
- Different derived types in the same program

## Function Return Type Inference

The `function_returns.f` example shows:
- Intrinsic function return types: `sin(x)` → `real(8)`
- String functions: `len_trim(text)` → `integer`
- Math functions: `sqrt(number)` → `real(8)`
- Future: User-defined function inference from definitions

## Running the Examples

```bash
fortran arrays.f
fortran derived_types.f
fortran function_returns.f
```

The CLI tool will automatically:
1. Detect array literals and field access patterns
2. Infer function return types from calls
3. Generate proper Fortran declarations
4. Compile and run the code

Note: These examples use the new modular type inference system (Phase 8) which provides more accurate and comprehensive type detection than the basic system.