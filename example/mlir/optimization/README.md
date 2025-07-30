# MLIR Optimization Case Studies

These examples demonstrate various optimization techniques and their effects on generated MLIR code.

## Examples

1. **constant_folding.f90** - Constant folding and propagation
2. **dead_code_elimination.f90** - Dead code elimination
3. **loop_optimization.f90** - Loop optimizations (unrolling, vectorization)
4. **function_inlining.f90** - Function inlining optimization
5. **algebraic_simplification.f90** - Algebraic simplifications

## Running Optimization Examples

```bash
# Generate unoptimized MLIR
fpm run fortran -- --compile --debug-codegen example.f90 > unoptimized.mlir

# Generate optimized MLIR (optimization enabled by default in compile mode)
fpm run fortran -- --compile --optimize --debug-codegen example.f90 > optimized.mlir

# Compare the two versions
diff unoptimized.mlir optimized.mlir
```

## Optimization Passes

The MLIR backend applies several optimization passes:

1. **Canonicalization** - Simplify operations to canonical forms
2. **Constant Folding** - Evaluate constant expressions at compile time
3. **Dead Code Elimination** - Remove unused computations
4. **Common Subexpression Elimination** - Eliminate redundant computations
5. **Loop Optimizations** - Optimize loop structures

## Expected Optimizations

### Before Optimization
```mlir
%c2 = arith.constant 2 : i32
%c3 = arith.constant 3 : i32
%0 = arith.addi %c2, %c3 : i32
```

### After Optimization
```mlir
%c5 = arith.constant 5 : i32
```

## Performance Analysis

Each example includes:
- Original unoptimized code
- Expected optimizations
- Performance measurements
- MLIR output comparison
