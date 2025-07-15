# *Lazy Fortran* Compiler Frontend TODO

## ✅ PHASE 8 COMPLETE: Dialect-Agnostic AST with JSON Workflow

**MAJOR MILESTONE ACHIEVED**: Core modules are now completely dialect-agnostic with full JSON pipeline support.

### ✅ Recently Completed:
- **Dialect-agnostic core modules**: codegen_core, debug_utils, semantic_analyzer
- **Unified type inference**: Available to all dialects through core assignment_node
- **Complete JSON workflow**: --from-tokens, --from-ast, --from-semantic CLI options
- **JSON deserializer**: Made dialect-agnostic (creates core program_node)
- **CLI integration**: All JSON input options working end-to-end
- **Test coverage**: Unit tests for all components, integration tests for CLI
- **Architecture compliance**: Core modules handle common features, dialects add specializations

### ⚠️ CRITICAL NOTE: Lazy Fortran Program vs Module Decision
**TODO**: Implement deferred program/module decision in semantic analyzer for lazy fortran:
- **Parser must be permissive**: Allow any statements valid in either program OR module context
- **Semantic analyzer decides**: Based on content analysis (executable statements → program, only procedures/declarations → module)  
- **File basename becomes name**: program/module name derived from input filename
- **Architecture requirement**: Core modules stay dialect-agnostic, decision logic only in lazy fortran semantic analysis

## CRITICAL ARCHITECTURAL ISSUE ⚠️ (Future Phase)

**ROOT CAUSE IDENTIFIED**: The parser violates Fortran 95 standard by processing **multi-line program units** as individual **line-by-line statements**.

### What's Fundamentally Wrong:
1. **Parser Architecture Violation**: Function definitions are **program units** (Fortran 95 section 7), not statements
2. **Line-by-line Processing**: Current parser calls `parse_statement()` on each line individually
3. **Multi-line Construct Ignorance**: Functions span multiple lines but parser doesn't recognize this
4. **Standard Violation**: Fortran 95 standard requires program unit parsing, not statement parsing

### Evidence from Standards:
According to `doc/standard/Fortran95.md`:
- **Program units include**: main program, external procedures, modules, block data  
- **Functions are program units**: `function name(args) ... end function name`
- **Not statements**: Functions are NOT executable statements

### Current Broken Flow:
```fortran
! Input:
real function compute(x)  <- Should be parsed as ONE function definition
  real :: x               <- Should be part of function body
  compute = x * x         <- Should be part of function body  
end function              <- Should be part of function definition

! Current parser (WRONG):
Line 1: parse_statement("real function compute(x)") → literal_node
Line 2: parse_statement("real :: x") → assignment_node  
Line 3: parse_statement("compute = x * x") → assignment_node
Line 4: parse_statement("end function") → unknown_node

! Required parser (CORRECT):
All lines: parse_program_unit(all_tokens) → function_def_node
```

## RECOVERY PLAN (Based on doc/plan/AST.md)

### Stage 1: Emergency Fix (IMMEDIATE - Today)
**Goal**: Get basic function definitions working

**Tasks**:
1. **Fix Program Unit Detection** in `src/frontend/parser/parser_core.f90`
   - Add `find_program_unit_boundary()` function
   - Detect function definitions across multiple lines
   - Group tokens by program unit, not by line

2. **Fix Code Generation** in `src/frontend/codegen/codegen_core.f90`
   - Remove bogus `0` statement labels from unrecognized nodes
   - Fix `generate_code_function_def()` to handle proper function structure
   - Generate valid Fortran syntax

3. **Quick Test**:
   - Simple function definition should parse and generate valid Fortran
   - `test_step1_single_file` should start passing

### Stage 2: Architecture Compliance (URGENT - This Week)
**Goal**: Implement proper Fortran 95 program unit parsing

**Tasks**:
1. **Implement Program Unit Parser** (new architecture)
   ```fortran
   ! Replace frontend.f90 line-by-line parsing with:
   function parse_program_unit(tokens) result(unit)
   function is_function_definition(parser) result(is_func)
   function is_subroutine_definition(parser) result(is_sub)
   function is_program_definition(parser) result(is_prog)
   ```

2. **Add Proper AST Nodes** for Fortran 95 compliance
   ```fortran
   type, extends(ast_node) :: program_unit_node
   type, extends(program_unit_node) :: function_def_node
   type, extends(program_unit_node) :: subroutine_def_node
   type, extends(program_unit_node) :: main_program_node
   ```

3. **Multi-line Token Grouping**
   - Replace `parse_tokens()` line-by-line approach
   - Implement proper program unit boundary detection
   - Handle `contains` sections correctly

### Stage 3: Type System Recovery (NEXT - Next Week)
**Goal**: Re-enable type inference with proper AST foundation

**Tasks**:
1. **Simplified Type Inference** (temporary)
   - Basic literal type inference (integer → integer, real → real(8))
   - Simple assignment type propagation
   - Function return type inference

2. **Fix Hindley-Milner Implementation**
   - Debug existing type system issues
   - Add proper function type inference
   - Re-enable semantic analysis gradually

3. **Test Type Inference**
   - All `test_step1_single_file` tests should pass
   - Function signature enhancement should work
   - Parameter type inference with `intent(in)` should work

### Stage 4: Complete Standard Compliance (FUTURE)
**Goal**: Full Fortran 95 standard compliance

**Tasks**:
1. **Complete Program Unit Support**
   - Module definitions
   - Block data
   - Subroutines
   - Nested contains sections

2. **Advanced Type System**
   - Bidirectional type checking
   - Array type inference
   - User-defined types

3. **Production Quality**
   - Error recovery
   - Helpful diagnostics
   - Performance optimization

## IMPLEMENTATION PRIORITY

### P0 (CRITICAL - IMMEDIATE):
1. **REMOVE ALL DIALECT-SPECIFIC NAMING FROM CORE MODULES**:
   - ⚠️ lexer_core, parser_core, semantic_analyzer, codegen_core must be dialect-agnostic
   - ⚠️ Rename lf_program_node → program_node (common to all dialects)
   - ⚠️ Remove lf_assignment_node → just use assignment_node
   - ⚠️ Move any dialect-specific features to specialized modules ONLY
   - ⚠️ Core modules handle COMMON Fortran features only

### P0 (COMPLETED):
1. **WRITE MINIMAL UNIT TESTS FIRST** - One line tests for each feature! ✓
2. **Fix `generate_code_polymorphic`** to never generate `0` ✓
3. **Fix `parse_function_definition`** to handle multi-line constructs ✓
4. **Test basic function parsing** with simple examples ✓
5. **FIX SEMANTIC ANALYZER DOUBLE FREE** ✓ (Fixed by simplifying initialization)

### COMPLETED TODAY:
- Created atomic unit tests for lexer, parser, and codegen
- Fixed parser to handle function parameters
- Fixed code generation to not declare function name as variable
- Fixed semantic analyzer initialization (removed problematic builtins)
- Added proper test structure documentation in CLAUDE.md
- ✓ Implemented JSON to AST deserialization (json_reader.f90)
- ✓ Created comprehensive tests for JSON workflows
- ✓ Fixed lf_program_node code generation in polymorphic dispatcher

### P1 (URGENT - This Week):
1. **Implement JSON input/output for all stages**:
   - ✓ JSON to tokens (basic implementation)
   - ✓ Tokens to parser (working)
   - ✓ JSON to AST deserialization (completed!)
   - ⚠️ AST to semantic analyzer (needs implementation)
   - ⚠️ Annotated AST to codegen (needs implementation)
   - ⚠️ CLI options: --from-tokens, --from-ast, --from-semantic
3. **Implement program unit parser** architecture
4. **Add proper AST nodes** for Fortran 95 constructs
5. **Replace line-by-line parsing** with program unit parsing

### P2 (HIGH - Next Week):
1. **Re-enable type inference** with simplified approach
2. **Fix semantic analysis** for function definitions
3. **Complete test suite** recovery

### P3 (MEDIUM - Future):
1. **Full Fortran 95 compliance** testing
2. **Module system** integration
3. **Advanced type features**

## CURRENT STATUS

### ✅ COMPLETED:
- **Phases 0-13**: Basic architecture, lexer, parser foundations
- **Cache system**: Fixed module conflict issues
- **Test infrastructure**: Organized test structure

### ❌ BROKEN:
- **Parser**: Line-by-line instead of program unit parsing
- **Code generation**: Generates invalid Fortran with statement labels
- **Type inference**: Complex inference disabled due to crashes
- **Function handling**: Multi-line constructs not recognized

### 🔧 IN PROGRESS:
- **Parser redesign**: Moving to program unit architecture
- **Code generation**: Fixing AST node handling
- **Type system**: Simplifying for reliability

## TEST METRICS

### Current:
- **24/37 tests passing** (65% pass rate)
- **test_step1_single_file**: 0/6 tests passing
- **test_runner_comprehensive**: FIXED (cache clearing)

### Target (Stage 1):
- **30/37 tests passing** (80% pass rate)
- **test_step1_single_file**: 3/6 tests passing
- **Basic function definitions**: Working

### Target (Stage 2): 
- **35/37 tests passing** (95% pass rate)
- **test_step1_single_file**: 6/6 tests passing
- **Complex multi-line programs**: Working

### Target (Stage 3):
- **37/37 tests passing** (100% pass rate)
- **Full type inference**: Working
- **Production ready**: Frontend complete

## CRITICAL SUCCESS FACTORS

1. **Follow Fortran 95 Standard**: No shortcuts, proper program unit parsing
2. **Fix Architecture**: No line-by-line parsing for multi-line constructs
3. **Proper AST Nodes**: Complete representation of language constructs
4. **Valid Code Generation**: Generated Fortran must compile correctly
5. **Incremental Progress**: Test each stage thoroughly before proceeding

**ULTIMATE GOAL**: Restore basic functionality where simple .f files with function definitions convert to valid .f90 files with proper type inference, building toward a complete, standard-compliant compiler frontend.