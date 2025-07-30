# Backend API Reference

This document provides comprehensive API reference for the backend system of the Fortran compiler, including interface specifications, implementation guidelines, and usage examples.

## Core Backend Interface

### `backend_t` (Abstract Base Type)

The abstract base type for all backend implementations.

```fortran
type, abstract :: backend_t
contains
    procedure(generate_code_interface), deferred :: generate_code
    procedure(get_name_interface), deferred :: get_name
    procedure(get_version_interface), deferred :: get_version
end type backend_t
```

#### Abstract Procedures

##### `generate_code`

**Signature:**
```fortran
subroutine generate_code(this, arena, backend_opts, output_code, success, error_message)
    class(backend_t), intent(inout) :: this
    type(ast_arena_t), intent(in) :: arena
    type(backend_options_t), intent(in) :: backend_opts
    character(len=:), allocatable, intent(out) :: output_code
    logical, intent(out) :: success
    character(len=:), allocatable, intent(out) :: error_message
end subroutine generate_code_interface
```

**Purpose:** Main entry point for code generation from AST

**Parameters:**
- `this`: Backend instance
- `arena`: AST arena containing parsed program structure
- `backend_opts`: Configuration options for code generation
- `output_code`: Generated code output (format depends on backend)
- `success`: Indicates whether generation succeeded
- `error_message`: Error details if generation failed

**Notes:**
- Implementation must handle all AST node types gracefully
- Should validate input AST before generation
- Error messages should be descriptive and actionable

##### `get_name`

**Signature:**
```fortran
function get_name(this) result(name)
    class(backend_t), intent(in) :: this
    character(len=:), allocatable :: name
end function get_name_interface
```

**Purpose:** Returns human-readable backend name

**Returns:** Backend name string (e.g., "MLIR", "Fortran", "C")

##### `get_version`

**Signature:**
```fortran
function get_version(this) result(version)
    class(backend_t), intent(in) :: this
    character(len=:), allocatable :: version
end function get_version_interface
```

**Purpose:** Returns backend version information

**Returns:** Version string (e.g., "1.0.0", "LLVM-18.0.0")

## Backend Options Type

### `backend_options_t`

Configuration structure for backend behavior.

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

#### Option Descriptions

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `optimize` | logical | `.false.` | Enable optimization passes |
| `debug_info` | logical | `.false.` | Include debugging information |
| `compile_mode` | logical | `.false.` | Generate object code instead of source |
| `generate_llvm` | logical | `.false.` | Use LLVM-compatible dialects (MLIR) |
| `generate_executable` | logical | `.false.` | Generate executable binary |
| `link_runtime` | logical | `.false.` | Link with runtime libraries |
| `enable_ad` | logical | `.false.` | Enable automatic differentiation |
| `generate_gradients` | logical | `.false.` | Generate gradient functions |
| `ad_annotations` | logical | `.false.` | Add AD annotations to output |
| `validate_gradients` | logical | `.false.` | Validate generated gradients |
| `target` | string | empty | Target architecture specification |
| `output_file` | string | empty | Output file path |

#### Usage Example

```fortran
type(backend_options_t) :: opts

! Configure for optimized MLIR compilation
opts%optimize = .true.
opts%compile_mode = .true.
opts%generate_llvm = .true.
opts%target = "x86_64-linux-gnu"
opts%output_file = "program.o"
```

## MLIR Backend Implementation

### `mlir_backend_t`

MLIR backend extends the abstract `backend_t` interface.

```fortran
type, extends(backend_t) :: mlir_backend_t
    integer :: ssa_counter = 0
    logical :: use_standard_dialects = .false.
    logical :: enable_ad = .false.
contains
    procedure :: generate_code => mlir_generate_code
    procedure :: get_name => mlir_get_name
    procedure :: get_version => mlir_get_version
    procedure :: reset_ssa_counter
    procedure :: next_ssa_value
end type mlir_backend_t
```

#### Public Procedures

##### `reset_ssa_counter`

**Signature:**
```fortran
subroutine reset_ssa_counter(this)
    class(mlir_backend_t), intent(inout) :: this
end subroutine reset_ssa_counter
```

**Purpose:** Reset SSA value counter to 0

**Usage:** Call before generating new MLIR module

##### `next_ssa_value`

**Signature:**
```fortran
function next_ssa_value(this) result(ssa_name)
    class(mlir_backend_t), intent(inout) :: this
    character(len=:), allocatable :: ssa_name
end function next_ssa_value
```

**Purpose:** Generate unique SSA value name

**Returns:** SSA value string (e.g., "%0", "%1", "%2")

**Notes:** Automatically increments internal counter

#### Private Procedures

##### `mlir_generate_code`

Implementation of abstract `generate_code` procedure.

**Process:**
1. Initialize MLIR backend state
2. Generate module header with dialect imports
3. Traverse AST and emit MLIR for each node
4. Apply optimization passes if enabled
5. Return generated MLIR code

##### `generate_mlir_module`

**Signature:**
```fortran
function generate_mlir_module(backend, arena, backend_opts) result(mlir)
    class(mlir_backend_t), intent(inout) :: backend
    type(ast_arena_t), intent(in) :: arena
    type(backend_options_t), intent(in) :: backend_opts
    character(len=:), allocatable :: mlir
end function generate_mlir_module
```

**Purpose:** Generate complete MLIR module from AST

##### `generate_mlir_node`

**Signature:**
```fortran
recursive function generate_mlir_node(backend, arena, node_index, indent_level) result(mlir)
    class(mlir_backend_t), intent(inout) :: backend
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: node_index
    integer, intent(in) :: indent_level
    character(len=:), allocatable :: mlir
end function generate_mlir_node
```

**Purpose:** Generate MLIR for specific AST node

**Parameters:**
- `node_index`: Index of node in AST arena
- `indent_level`: Current indentation level for formatting

#### Type Mapping Functions

##### `fortran_to_mlir_type`

**Signature:**
```fortran
function fortran_to_mlir_type(fortran_type, kind_value) result(mlir_type)
    character(len=*), intent(in) :: fortran_type
    integer, intent(in) :: kind_value
    character(len=:), allocatable :: mlir_type
end function fortran_to_mlir_type
```

**Purpose:** Convert Fortran types to MLIR type representation

**Type Mappings:**
- `integer(4)` → `i32`
- `integer(8)` → `i64`
- `real(4)` → `f32`
- `real(8)` → `f64`
- `logical` → `i1`
- `character` → `!fir.char<1>`

## Fortran Backend Implementation

### `fortran_backend_t`

Fortran backend extends the abstract `backend_t` interface.

```fortran
type, extends(backend_t) :: fortran_backend_t
contains
    procedure :: generate_code => fortran_generate_code
    procedure :: get_name => fortran_get_name
    procedure :: get_version => fortran_get_version
end type fortran_backend_t
```

#### Implementation Details

The Fortran backend generates standardized Fortran code from the AST, preserving semantic equivalence while applying consistent formatting and style rules.

**Key Features:**
- Maintains Fortran language semantics
- Applies consistent indentation (4 spaces)
- Handles all AST node types
- Preserves variable scope and visibility
- Generates modern Fortran syntax

## Backend Factory

### `create_backend`

**Signature:**
```fortran
function create_backend(backend_type, backend_opts) result(backend)
    character(len=*), intent(in) :: backend_type
    type(backend_options_t), intent(in) :: backend_opts
    class(backend_t), allocatable :: backend
end function create_backend
```

**Purpose:** Factory function to create backend instances

**Parameters:**
- `backend_type`: Backend identifier ("mlir", "fortran", "c")
- `backend_opts`: Configuration options

**Returns:** Allocated backend instance

**Example:**
```fortran
class(backend_t), allocatable :: backend
type(backend_options_t) :: opts

opts%compile_mode = .true.
backend = create_backend("mlir", opts)
```

### `get_backend_type_from_options`

**Signature:**
```fortran
function get_backend_type_from_options(backend_opts) result(backend_type)
    type(backend_options_t), intent(in) :: backend_opts
    character(len=:), allocatable :: backend_type
end function get_backend_type_from_options
```

**Purpose:** Determine backend type from options

**Logic:**
- If `compile_mode = .true.` → "mlir"
- Otherwise → "fortran"

## AST Integration

### AST Arena Access

The backend system integrates with the AST arena to access parsed program structure:

```fortran
use ast_factory, only: ast_arena_t, get_node_by_index
use ast_nodes, only: ast_node_t, program_node_t, function_node_t
```

#### Common AST Operations

##### Get Node by Index
```fortran
class(ast_node_t), pointer :: node
node => get_node_by_index(arena, node_index)
```

##### Node Type Checking
```fortran
select type (node)
type is (program_node_t)
    ! Handle program node
type is (function_node_t)
    ! Handle function node
class default
    ! Handle unknown node type
end select
```

## Error Handling

### Error Reporting Standards

Backends should follow consistent error reporting:

#### Error Message Format
```
[BACKEND_NAME] ERROR: <description>
Location: <file>:<line>:<column>
Context: <relevant_code_snippet>
Suggestion: <actionable_advice>
```

#### Error Categories
- **Syntax Errors**: Invalid AST structure
- **Type Errors**: Unsupported type combinations
- **Feature Errors**: Unimplemented language features
- **System Errors**: Tool or resource failures

#### Example Error Handling
```fortran
if (.not. success) then
    error_message = "MLIR ERROR: Unsupported array syntax" // new_line('a') // &
                   "Location: " // trim(node%source_location) // new_line('a') // &
                   "Suggestion: Use standard array declaration syntax"
    return
end if
```

## Testing Backend Implementations

### Required Test Categories

#### Unit Tests
- Basic code generation for each AST node type
- Error handling for invalid inputs
- Option configuration effects

#### Integration Tests
- Complete program compilation
- Backend comparison (semantic equivalence)
- Tool chain integration (MLIR/LLVM tools)

#### Performance Tests
- Compilation time benchmarks
- Memory usage analysis
- Optimization effectiveness

### Test Structure Example

```fortran
! File: test/backend/test_mlir_backend.f90
program test_mlir_backend
    use backend_interface
    use ast_factory

    type(backend_options_t) :: opts
    class(backend_t), allocatable :: backend
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: output_code
    logical :: success
    character(len=:), allocatable :: error_message

    ! Configure backend
    opts%compile_mode = .true.
    backend = create_backend("mlir", opts)

    ! Generate code
    call backend%generate_code(arena, opts, output_code, success, error_message)

    ! Validate results
    if (.not. success) then
        print *, "ERROR:", error_message
        stop 1
    end if

    print *, "Generated MLIR:"
    print *, output_code
end program test_mlir_backend
```

## Extension Guidelines

### Adding New Backends

1. **Create Backend Type**: Extend `backend_t` abstract type
2. **Implement Required Procedures**: `generate_code`, `get_name`, `get_version`
3. **Register with Factory**: Add to `create_backend` function
4. **Add Comprehensive Tests**: Unit, integration, and performance tests
5. **Update Documentation**: Add to this API reference

### Adding New Options

1. **Extend `backend_options_t`**: Add new option fields
2. **Update CLI Parser**: Handle new command-line flags
3. **Implement Option Logic**: Use options in backend implementations
4. **Document Options**: Add to option reference table
5. **Add Tests**: Verify option behavior

### Adding AST Node Support

1. **Handle in `generate_mlir_node`**: Add new `type is` case
2. **Implement Generation Function**: Create `generate_mlir_<node_type>`
3. **Map Types**: Update `fortran_to_mlir_type` if needed
4. **Add Tests**: Verify node generation
5. **Update All Backends**: Ensure consistency across backends

## Best Practices

### Code Generation

1. **Consistent Formatting**: Use consistent indentation and spacing
2. **Error Recovery**: Handle partial failures gracefully
3. **Validation**: Validate generated code with external tools
4. **Performance**: Minimize memory allocations in hot paths

### Maintainability

1. **Modular Design**: Keep functions focused and small
2. **Clear Naming**: Use descriptive function and variable names
3. **Documentation**: Document complex algorithms and design decisions
4. **Testing**: Maintain high test coverage for all code paths

### Compatibility

1. **Version Compatibility**: Handle different MLIR/LLVM versions
2. **Platform Support**: Test on multiple operating systems
3. **Tool Dependencies**: Gracefully handle missing external tools
4. **Backward Compatibility**: Preserve existing API contracts

## Troubleshooting

### Common Issues

#### Backend Not Found
**Symptom:** `create_backend` returns null
**Solution:** Check backend type string, ensure backend is registered

#### MLIR Validation Failures
**Symptom:** `mlir-opt --verify-each` reports errors
**Solution:** Check SSA value uniqueness, dialect imports, type consistency

#### Memory Leaks
**Symptom:** Increasing memory usage during compilation
**Solution:** Review allocatable variable cleanup, use profiling tools

#### Performance Degradation
**Symptom:** Slow compilation times
**Solution:** Profile code generation, optimize AST traversal, reduce allocations

### Debug Techniques

1. **Enable Debug Output**: Use `--debug-codegen` flag
2. **Isolate Problems**: Test with minimal input programs
3. **Compare Backends**: Verify against known-good backend
4. **Use External Tools**: Validate with MLIR/LLVM tools

For more detailed debugging information, see the [MLIR Debugging Techniques](mlir-debugging.md) guide.
