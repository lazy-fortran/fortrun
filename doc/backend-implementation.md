# MLIR Backend Implementation Guide

This document provides a comprehensive guide for developers working on the MLIR backend implementation of the Fortran compiler.

## Architecture Overview

The MLIR backend follows a multi-stage compilation pipeline:

```
Fortran Source → AST → HLFIR → FIR → LLVM IR → Object Code/Executable
```

Each stage is accessible via CLI flags:
- `--emit-hlfir`: Stop after HLFIR generation
- `--emit-fir`: Lower HLFIR to FIR and stop
- `--emit-llvm`: Lower to LLVM IR and stop
- `--compile`: Complete pipeline to object/executable

### Key Components

1. **AST Factory** (`src/ast/ast_factory.f90`) - Creates AST nodes from parsed Fortran code
2. **MLIR Backend** (`src/backend/mlir/mlir_backend.f90`) - Main MLIR code generation
3. **Backend Interface** (`src/backend/backend_interface.f90`) - Abstract backend interface
4. **Pipeline Orchestrator** (`src/pipeline/orchestrator.f90`) - Manages compilation pipeline

## Backend Implementation Details

### MLIR Backend Type (`mlir_backend_t`)

The MLIR backend extends the abstract `backend_t` interface:

```fortran
type, extends(backend_t) :: mlir_backend_t
    integer :: ssa_counter = 0  ! SSA value counter
    logical :: use_standard_dialects = .false.  ! Use standard MLIR dialects for LLVM lowering
    logical :: enable_ad = .false.  ! Enable automatic differentiation
contains
    procedure :: generate_code => mlir_generate_code
    procedure :: get_name => mlir_get_name
    procedure :: get_version => mlir_get_version
    procedure :: reset_ssa_counter
    procedure :: next_ssa_value
end type mlir_backend_t
```

### Key Procedures

#### `generate_code`
Main entry point for MLIR code generation. Takes an AST and generates MLIR output.

#### `generate_mlir_module`
Creates the top-level MLIR module structure with proper dialect imports and optimization annotations.

#### Node-Specific Generation
- `generate_mlir_program` - Program/main function
- `generate_mlir_function` - Function definitions with AD attributes
- `generate_mlir_declaration` - Variable declarations (FIR alloca or memref.alloca)
- `generate_mlir_assignment` - Assignment operations
- `generate_mlir_binary_op` - Arithmetic operations using arith dialect
- `generate_mlir_do_loop` - Loop constructs using SCF dialect
- `generate_mlir_if` - Conditional statements using SCF dialect

### MLIR Dialect Usage

#### FIR Dialect (Default)
Used for Fortran-specific operations:
```mlir
%1 = fir.alloca i32 {bindc_name = "x"}
fir.call @function() : () -> ()
```

#### Standard Dialects (LLVM Lowering)
Used when `generate_llvm` option is enabled:
```mlir
%1 = memref.alloca() : memref<i32>
%2 = arith.constant 42 : i32
func.func @main() -> i32
```

### Type System Integration

The backend provides comprehensive type mapping:

```fortran
function fortran_to_mlir_type(fortran_type, kind_value) result(mlir_type)
```

| Fortran Type | MLIR Type |
|--------------|-----------|
| `integer(4)` | `i32` |
| `integer(8)` | `i64` |
| `real(4)` | `f32` |
| `real(8)` | `f64` |
| `logical` | `i1` |
| `character` | `!fir.char<1>` |

### Automatic Differentiation Support

When `enable_ad` is true, functions are annotated with Enzyme attributes:

```mlir
func.func @function() -> f32 attributes {enzyme.differentiable} {
  // function body
}
```

## Backend Options

Configure backend behavior through `backend_options_t`:

```fortran
type :: backend_options_t
    logical :: optimize = .false.
    logical :: debug_info = .false.
    logical :: compile_mode = .false.
    logical :: generate_llvm = .false.
    logical :: generate_executable = .false.
    logical :: link_runtime = .false.
    logical :: enable_ad = .false.
    logical :: generate_gradients = .false.
    logical :: ad_annotations = .false.
    logical :: validate_gradients = .false.
    character(len=:), allocatable :: target
    character(len=:), allocatable :: output_file
end type backend_options_t
```

### Option Effects

- `optimize` - Enable optimization passes and add optimization comments
- `generate_llvm` - Use standard MLIR dialects instead of FIR for LLVM lowering
- `enable_ad` - Add Enzyme AD attributes to functions
- `compile_mode` - Generate object code instead of standardized Fortran

## Adding New AST Node Support

To add support for a new AST node type:

1. **Add node type** to `generate_mlir_node` select type construct
2. **Implement generation function** following naming convention `generate_mlir_<node_type>`
3. **Add appropriate MLIR dialect operations**
4. **Update type mapping** if new types are introduced
5. **Add tests** in `test/mlir/`

Example for a new node type:

```fortran
! In generate_mlir_node
type is (new_node_type)
    mlir = generate_mlir_new_node(backend, arena, node, indent_str)

! New generation function
function generate_mlir_new_node(backend, arena, node, indent_str) result(mlir)
    class(mlir_backend_t), intent(inout) :: backend
    type(ast_arena_t), intent(in) :: arena
    type(new_node_type), intent(in) :: node
    character(len=*), intent(in) :: indent_str
    character(len=:), allocatable :: mlir

    ! Generate appropriate MLIR for this node
    mlir = indent_str // "// New node implementation" // new_line('a')
end function generate_mlir_new_node
```

## Testing

### MLIR Test Structure

Tests are organized in `test/mlir/`:
- `test_basic_generation.f90` - Basic MLIR generation
- `test_ast_mapping.f90` - AST to MLIR mapping
- `test_types.f90` - Type system integration
- `test_optimization.f90` - Optimization passes
- `test_llvm_lowering.f90` - LLVM IR lowering
- `test_enzyme_ad.f90` - Automatic differentiation

### Running Tests

```bash
# Run all MLIR tests
make test-mlir

# Run with MLIR validation
make test-mlir-validate

# Run specific test
fpm test test_basic_generation
```

### Test Harness

The MLIR test harness provides:
- Automated test execution
- MLIR validation with `mlir-opt`
- Performance measurement
- JUnit XML output for CI

## Common Issues and Solutions

### MLIR Validation Errors

**Problem**: `mlir-opt --verify-each` fails on generated MLIR

**Solutions**:
1. Check SSA value uniqueness - use `backend%next_ssa_value()`
2. Ensure proper dialect declarations in module header
3. Verify type consistency in operations
4. Check for balanced braces `{}` in function definitions

### FIR vs Standard Dialects

**Problem**: Generated MLIR uses FIR dialect but LLVM lowering expected

**Solution**: Set `generate_llvm = .true.` in backend options to use standard dialects:
```fortran
backend_opts%generate_llvm = .true.
```

### Enzyme AD Integration

**Problem**: AD attributes not appearing in generated MLIR

**Solution**: Enable AD in backend options:
```fortran
backend_opts%enable_ad = .true.
backend_opts%generate_gradients = .true.
```

### Performance Issues

**Problem**: MLIR backend slower than Fortran backend

**Debugging**:
1. Use performance benchmarks: `make benchmark`
2. Profile with external tools
3. Check for inefficient AST traversal
4. Optimize MLIR generation algorithms

## Debugging Techniques

### Debug Output

Enable debug output to see generated MLIR:
```bash
fortran --compile --debug-codegen input.f90
```

### MLIR Tools

Use MLIR tools for validation and analysis:

```bash
# Validate MLIR syntax
mlir-opt --verify-each output.mlir

# Convert to LLVM IR
mlir-translate --mlir-to-llvmir output.mlir

# Optimize MLIR
mlir-opt --convert-func-to-llvm --finalize-memref-to-llvm output.mlir
```

### AST Inspection

Inspect AST structure before MLIR generation:
```bash
fortran --debug-ast input.f90
```

## Contributing

### Code Style

- Follow existing patterns in `mlir_backend.f90`
- Use descriptive function names: `generate_mlir_<construct>`
- Add proper error handling and validation
- Include comprehensive tests for new features

### Documentation

- Update this guide when adding new features
- Add inline comments for complex MLIR generation
- Document new backend options and their effects
- Include examples in function documentation

### Testing Requirements

- All new AST node support must include tests
- MLIR output must validate with `mlir-opt --verify-each`
- Performance impact should be measured with benchmarks
- Integration tests should cover end-to-end compilation

## References

- [MLIR Language Reference](https://mlir.llvm.org/docs/LangRef/)
- [FIR Dialect Documentation](https://flang.llvm.org/docs/FIRLangRef.html)
- [Enzyme Automatic Differentiation](https://enzyme.mit.edu/)
- [LLVM Project Documentation](https://llvm.org/docs/)

For questions or issues, see the troubleshooting section or consult the test suite for working examples.
