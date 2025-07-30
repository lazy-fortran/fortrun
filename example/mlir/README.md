# MLIR Backend Examples

This directory contains comprehensive examples demonstrating the MLIR backend capabilities of the Fortran compiler.

## Directory Structure

- `basic/` - Basic Fortran to FIR/MLIR examples
- `autodiff/` - Automatic differentiation examples using Enzyme
- `gpu/` - GPU offload examples (future)
- `optimization/` - Optimization case studies

## Running Examples

### Basic Usage

```bash
# Generate MLIR output
fpm run fortran -- --compile --debug-codegen example.f90

# Compile to object file
fpm run fortran -- --compile example.f90

# Compile to executable
fpm run fortran -- --compile -o example example.f90
```

### With Optimization

```bash
# Enable optimization
fpm run fortran -- --compile --optimize example.f90
```

### With Automatic Differentiation

```bash
# Enable AD
fpm run fortran -- --compile --enable-ad gradient_example.f90
```

## Validation

All examples can be validated using MLIR tools:

```bash
# Validate MLIR syntax
mlir-opt --verify-each output.mlir

# Convert to LLVM IR
mlir-translate --mlir-to-llvmir output.mlir -o output.ll

# Compile with LLVM
llc output.ll -o output.o
clang output.o -o executable
```

## Test All Examples

```bash
# Run all MLIR examples through test harness
make test-mlir

# Run with validation
make test-mlir-validate
```

Each subdirectory contains its own README with specific instructions and expected outputs.
