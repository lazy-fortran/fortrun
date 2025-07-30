# Basic Fortran to MLIR Examples

These examples demonstrate basic Fortran language constructs and their MLIR/FIR representation.

## Examples

1. **hello_world.f90** - Simple program with print statement
2. **variables.f90** - Variable declarations and assignments
3. **arithmetic.f90** - Arithmetic operations and expressions
4. **functions.f90** - Function definitions and calls
5. **loops.f90** - Do loops and control flow
6. **conditionals.f90** - If statements and logical operations
7. **arrays.f90** - Array declarations and operations

## Running Examples

```bash
# Generate MLIR for hello world
fpm run fortran -- --compile --debug-codegen hello_world.f90 > hello_world.mlir

# Validate generated MLIR
mlir-opt --verify-each hello_world.mlir

# Compile to executable
fmp run fortran -- --compile -o hello_world hello_world.f90
./hello_world
```

## Expected MLIR Patterns

### Variable Declaration
```mlir
// Fortran: integer :: x = 42
%0 = fir.alloca i32 {bindc_name = "x"}
%1 = arith.constant 42 : i32
fir.store %1 to %0 : !fir.ref<i32>
```

### Function Definition
```mlir
// Fortran: integer function add(a, b)
func.func @add(%arg0: i32, %arg1: i32) -> i32 {
  %0 = arith.addi %arg0, %arg1 : i32
  return %0 : i32
}
```

### Loop Construction
```mlir
// Fortran: do i = 1, 10
%c1 = arith.constant 1 : index
%c10 = arith.constant 10 : index
%c1_step = arith.constant 1 : index
scf.for %i = %c1 to %c10 step %c1_step {
  // loop body
}
```
