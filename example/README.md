# Fortran CLI Examples

This directory contains examples demonstrating various features of the Fortran CLI tool.

## Examples

### [hello/](hello/)
Basic "Hello, World!" program demonstrating minimum functionality.

### [calculator/](calculator/)
Simple calculator using local modules. Demonstrates local module dependency resolution.

### [precision/](precision/)
Examples showing modern precision defaults (double precision by default, implicit none).

### [interdependent/](interdependent/)
Complex example with multiple modules that depend on each other. Tests advanced local module resolution.

### [plotting/](plotting/)
Example using external dependencies (currently disabled in build).

## Running Examples

All examples can be run with:

```bash
fortran path/to/example.f90
```

For example:
```bash
fortran example/hello/hello.f90
fortran example/calculator/calculator.f90
fortran example/interdependent/main.f90
```

## Testing

All examples are automatically tested with:

```bash
fpm test test_examples
```

This ensures that all examples continue to work as the CLI tool evolves.