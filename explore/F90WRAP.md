# F90wrap Parser Analysis

## Overview
f90wrap is a tool for generating Python interfaces to Fortran code. Its parser provides valuable insights for handling Fortran syntax, especially function declarations and type information.

**Repository**: https://github.com/jameskermode/f90wrap

## Key Parser Components

### 1. AST-Based Architecture
- **fortran.py**: https://github.com/jameskermode/f90wrap/blob/master/f90wrap/fortran.py
  - Defines AST node classes (Module, Procedure, Function, Argument)
  - Each class represents a Fortran construct with its metadata
  - Function class extends Procedure with return value handling

### 2. Parser Implementation
- **parser.py**: https://github.com/jameskermode/f90wrap/blob/master/f90wrap/parser.py
  - Uses regular expressions for pattern matching
  - Function regex: `r'^((' + types + '|' + prefixes + r')\\s+)\*function'`
  - Case-insensitive matching with `re.IGNORECASE`

### 3. Transform Module
- **transform.py**: https://github.com/jameskermode/f90wrap/blob/master/f90wrap/transform.py
  - Post-processes the AST
  - Handles interface transformations
  - Updates access modifiers (public/private)

## Function Declaration Handling

### Pattern Matching Approach
```python
# Function declaration pattern
funct = re.compile(r'^((types|prefixes)\\s+)\*function', re.IGNORECASE)
```

### Key Features
1. **Type Extraction**: Matches optional type prefix (real, integer, etc.)
2. **Name Parsing**: Extracts function name after 'function' keyword
3. **Parameter List**: Parses comma-separated arguments in parentheses
4. **Return Value**: 
   - Explicit: `real function foo()`
   - Implicit: Function name used as return variable

### AST Structure for Functions
```python
class Function(Procedure):
    def __init__(self, ...):
        self.ret_val = Argument()  # Return value representation
        self.ret_val_doc = None    # Documentation
        self.arguments = []        # Parameter list
```

## Lessons for Our Type Inference

### 1. Function Signature Parsing
- Need to extract function name from declaration line
- Parse parameter list from parentheses
- Handle optional type prefix for return type

### 2. Type Handling
- Function return type can be explicit or implicit
- Parameters need type inference from usage
- Function name acts as return variable inside function body

### 3. Scope Management
- Functions create new scopes
- Parameters are part of function scope
- Return variable (function name) is in function scope

## Implementation Ideas

### Enhanced Function Declaration Detection
```fortran
! Current: Simple pattern matching
if (index(trimmed, 'function ') == 1) then

! Better: Extract function name and parameters
! Pattern: [type] function name(param1, param2, ...)
! Examples:
!   function foo(x)
!   real function bar(a, b)
!   integer(8) function baz()
```

### Function Parameter Tracking
1. When entering function, parse parameter list
2. Add parameters to function scope with unknown type
3. Infer types from usage within function
4. Special handling for function name as return variable

### Type Inference Enhancement
- Track which variables are function parameters
- Track which variable is the function return value
- Don't generate declarations for parameters (they go in function signature)
- Handle function name assignment differently from regular variables

## References
- Main repository: https://github.com/jameskermode/f90wrap
- Documentation: https://github.com/jameskermode/f90wrap/blob/master/docs/usage.rst
- Examples: https://github.com/jameskermode/f90wrap/tree/master/examples

## Key Takeaways
1. **Regex-based parsing** is effective for Fortran syntax
2. **AST representation** helps manage complex relationships
3. **Separate handling** for functions vs regular variables
4. **Multi-pass approach**: Parse first, transform later
5. **Explicit modeling** of return values and parameters