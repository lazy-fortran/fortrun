# Automatic Differentiation Examples

These examples demonstrate automatic differentiation capabilities using the Enzyme AD framework integrated with the MLIR backend.

## Examples

1. **simple_gradient.f90** - Basic gradient computation
2. **polynomial.f90** - Polynomial function differentiation
3. **mathematical_functions.f90** - Trigonometric and exponential functions
4. **optimization.f90** - Gradient-based optimization example
5. **neural_network.f90** - Simple neural network with backpropagation

## Running AD Examples

```bash
# Generate MLIR with AD annotations
fpm run fortran -- --compile --enable-ad --debug-codegen simple_gradient.f90

# Compile with AD enabled
fpm run fortran -- --compile --enable-ad -o gradient_program simple_gradient.f90

# Run the gradient computation
./gradient_program
```

## Expected MLIR Patterns

### Function with AD Attributes
```mlir
// Fortran function marked for differentiation
func.func @f(%arg0: f64) -> f64 attributes {enzyme.differentiable} {
  %0 = math.powf %arg0, %c2_f64 : f64
  return %0 : f64
}
```

### Gradient Function Call
```mlir
// Generated gradient computation
%gradient = call @__enzyme_autodiff @f, %x : (f64) -> f64
```

## Mathematical Background

Each example includes:
- Original function implementation
- Analytical derivative (for validation)
- Enzyme-generated derivative
- Comparison of results

## Validation

All AD examples include validation against analytical derivatives with tolerance checks.
