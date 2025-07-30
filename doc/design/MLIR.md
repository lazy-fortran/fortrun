**Design Document: HLFIR Text‑Based Workflow with Enzyme AD (Linked Program Execution)**

**1. Overview**

- Frontend emits HLFIR (High-Level Fortran IR) MLIR text files (.mlir) from the annotated Fortran AST.
- `mlir-opt` pipeline applies HLFIR optimizations, then lowers to FIR, and applies optional Enzyme AD.
- Lower FIR to CPU (LLVM IR → object) and GPU (SPIR‑V → binary) artifacts.
- Host driver links CPU object code and loads GPU kernels at runtime for dispatch and data exchange.

**2. Goals**

- Maintain text‑first workflow for debugging.
- Support reverse‑mode AD via Enzyme on HLFIR and FIR.
- Leverage HLFIR's higher-level abstractions for better optimization.
- Produce a single host executable plus GPU shader assets.
- Define clear linking and execution steps for primal and adjoint runs.
- Abstract backend architecture to support multiple code generation targets.
- Enable test-driven development with comprehensive test infrastructure.

**3. Backend Architecture Integration**

The MLIR backend will be integrated through a modular architecture with pipeline orchestration:

```text
Pipeline Orchestrator → AST Module → Backend Interface → [ Fortran Backend ]
                                                       → [ MLIR Backend   ]
                                                       → [ C Backend      ]
```

**Architecture Components:**

**Pipeline Orchestrator:**
- Handles backend selection based on CLI flags
- Routes `--compile` flag to MLIR backend
- Maintains default codegen backend for existing paths
- Manages compilation workflow and output generation

**AST Module:**
- Backend-agnostic AST representation
- Extracted from frontend as standalone module
- Shared by all backends for consistent processing
- No circular dependencies with frontend or backends

**Backend Interface:**
- Polymorphic dispatch based on compilation options
- Shared AST traversal utilities
- Backend-specific emission logic
- Test harness support for validation

**CLI Integration:**
- `fortran program.f90` - Uses default Fortran codegen backend (runs program)
- `fortran --emit-f90 program.f90` - Emit standardized Fortran (synonym for --standardize)
- `fortran --emit-hlfir program.f90` - Emit HLFIR MLIR text
- `fortran --emit-fir program.f90` - Emit FIR (lowered from HLFIR)
- `fortran --emit-llvm program.f90` - Emit LLVM IR (lowered from FIR)
- `fortran --compile program.f90` - Generate object file (via full pipeline)
- `fortran --compile -o program program.f90` - Generate executable (via full pipeline)

**4. High‑Level Pipeline**

```text
1. Frontend:             gen_hlfir  → module.mlir        # HLFIR generation
2. HLFIR Optimization:   mlir-opt   → module.hlfir.mlir  # HLFIR-level opts, elemental fusion
3. HLFIR→FIR Lowering:   mlir-opt   → module.fir.mlir    # convert-hlfir-to-fir
4. FIR Optimization:     mlir-opt   → module.opt.mlir    # FIR optimization, enzyme AD
5. CPU Lowering:         fir-to-llvm → module.ll        # FIR→LLVM IR
                         llc         → module.o         # LLVM IR→object
6. GPU Lowering:         mlir-opt    → module.spv        # FIR→SPIR‑V dialect→.spv
                         spirv-opt   → module.opt.spv    # optimize & validate
7. Linking:              c++ linker  → host_exec        # link module.o + RTL + driver
8. Deployment:           host_exec + module.opt.spv files
```

**5. Emit HLFIR Text**  

Emit your HLFIR MLIR in plain text for maximum debug visibility.  

- **HLFIR Documentation**: See the HLFIR specification → https://github.com/llvm/llvm-project/blob/main/flang/docs/HighLevelFIR.md
- **FIR Dialect Reference**: See the FIR dialect spec → https://mlir.llvm.org/docs/Dialects/FIR/  
- **Flang Tutorial**: `-emit-mlir` example → https://mlir.llvm.org/docs/FlangTutorial/#emit-mlir  

**Workflow**:  
1. **AST Mapping**: For each procedure in your semantically annotated AST, print a `module { ... }` with HLFIR ops.  
   - `fir.func` for functions (see `fir.func` docs)  
   - `hlfir.declare` for variable declarations with metadata  
   - `hlfir.elemental` for elemental operations on arrays
   - `hlfir.assign` for high-level assignments
   - `!hlfir.expr` types for expression values
   - `scf.for` for loops  
   - `fir.call` for calls  
   - `func.return` for returns  
2. **Expression Handling**: Use `!hlfir.expr` types to defer materialization of temporaries
3. **Autodiff Annotation**: If `-fad` is enabled, emit a separate `grad_` function with `call @__enzyme_autodiff`.  
4. **Example Snippet**:  
```mlir
// module.mlir - HLFIR representation
module {
  // Main entry
  fir.func @main() attributes {llvm.linkage = #llvm.linkage<"external">} {
    %0 = hlfir.declare "i" : !fir.ref<i32>
    ...
    func.return
  }

  // Kernel with autodiff using HLFIR
  fir.func @compute(%arg: !fir.ref<!fir.array<128xf32>>) attributes {llvm.linkage = #llvm.linkage<"external">} {
    %0 = hlfir.declare %arg : !fir.ref<!fir.array<128xf32>>

    // Use hlfir.elemental for array operations
    %1 = hlfir.elemental !fir.array<128xf32> {
    ^bb0(%i: index):
      %v = hlfir.designate %0 (%i) : (!fir.ref<!fir.array<128xf32>>, index) -> !fir.ref<f32>
      %val = fir.load %v : !fir.ref<f32>
      %result = arith.mulf %val, %val : f32
      hlfir.yield_element %result : f32
    }

    // High-level assignment
    hlfir.assign %1 to %0 : !hlfir.expr<!fir.array<128xf32>>, !fir.ref<!fir.array<128xf32>>
    func.return
  }

  // Gradient entry (Enzyme annotation)
  fir.func @grad_compute(%arg: !fir.ref<!fir.array<128xf32>>) {
    %g = call @__enzyme_autodiff(%arg) : (!fir.ref<!fir.array<128xf32>>) -> !fir.ref<!fir.array<128xf32>>
    func.return %g : !fir.ref<!fir.array<128xf32>>
  }
}
```

**Debug Tips**:  
- Validate text with `mlir-opt --verify-each`.  
- Round‑trip with `mlir-translate` to catch syntax issues.  

**Proceed to** **6. IR Optimization & AD**

- Run HLFIR passes first:
  - `--passes="canonicalize,hlfir-fuse,inline,canonicalize"`
- Then convert to FIR:
  - `--convert-hlfir-to-fir`
- Then run FIR optimization passes:
  - `--passes="canonicalize,fir-fuse,inline,canonicalize"`
  - Optional Enzyme: `--load-dialect-plugin=libEnzymeFIR.so --pass-pipeline="enzyme,canonicalize"`
- Output: `module.opt.mlir` containing both primal and adjoint FIR definitions.

**7. CPU Artifact Generation**

1. **FIR→LLVM IR**
   ```bash
   mlir-opt module.opt.mlir --convert-fir-to-llvm > module.ll
   ```
2. **LLVM IR→Object**
   ```bash
   clang -c module.ll -o module.o \        # or llc+gcc
         `llvm-config --cflags --ldflags --libs`
   ```
3. **Link**
   ```bash
   clang module.o fortran_rtl.o driver.o -o fortran_app \
         -lLLVM -lmlir_fir -lEnzymeFIR
   ```

**8. GPU Artifact Generation**

1. **FIR→SPIR‑V dialect**
   ```bash
   mlir-opt module.opt.mlir --convert-fir-to-gpu \
           --convert-gpu-to-spv > module.spv
   ```
2. **SPIR‑V Optimize & Validate**
   ```bash
   spirv-opt -O2 module.spv -o module.opt.spv
   spirv-val module.opt.spv
   ```
3. **Package**
   - Ship `module.opt.spv` alongside `fortran_app`, or embed as byte array.

**9. Host Driver Design**

- **Initialization**
  - Parse command‑line flags (`-fad`, `-acc`, GPU device, workgroup sizes).
  - Load Fortran runtime (I/O, coarrays, OpenMP host stubs).
- **Module Registration**
  - **CPU functions**: linked directly into `fortran_app` text segment.
  - **GPU kernels**: at startup, read `module.opt.spv` from disk or memory, call:
    ```c++
    VkShaderModule createShaderModule(VkDevice d, const void* data, size_t size);
    cl_program   createCLProgram(cl_context, size, data);
    ```
- **Data Movement**
  - Host allocates buffers (CPU or GPU) via runtime API: `allocate()`, `omp_target_alloc()`.
  - Copy in data for GPU kernels: `omp_target_memcpy()` or Vulkan buffer uploads.
- **Dispatch**
  - For each kernel (primal or adjoint), set up arguments.
  - Submit dispatch (`vkCmdDispatch` or `clEnqueueNDRangeKernel`).
  - Wait for completion and copy results back if needed.

**10. Adjoint Execution**

- If `-fad` used: application entry point calls `grad_main` instead of `main`.
- PRIMAL run followed by AD run can be sequenced automatically.
- Host driver provides accumulation buffers and hooks for gradient retrieval.

**11. Linked Program Workflow**

```text
# Build and link
./build.sh  # executes steps 1–7 above

# Run primal-only
./fortran_app input.dat

# Run with gradient
./fortran_app -fad input.dat
  → driver executes main() → grad_main()
  → outputs primal results + gradients

# GPU offload
./fortran_app -acc -device 0 input.dat
  → loads module.opt.spv, dispatches kernels
  → optionally calls grad_compute_kernel()
```

**12. Debugging & Validation**

- Inspect each `.mlir` with `mlir-opt --print-ir-after-all`.
- Round‑trip text→bytecode→text to catch serialization bugs:
  ```bash
  mlir-translate --mlir-to-bytecode module.opt.mlir -o m.bc
  mlir-translate --bytecode-to-mlir m.bc      > m2.mlir
  diff module.opt.mlir m2.mlir
  ```
- Use Enzyme's test suite to validate gradients at MLIR level.

**13. Implementation Phases**

The MLIR backend will be implemented in clearly defined phases:

**Phase 1: CPU-Only Implementation**
- Focus on simplest possible CPU target generation
- Basic HLFIR emission with conversion to FIR
- LLVM IR lowering and object file generation
- Executable linking with minimal runtime

**Phase 2: Optimization Support**
- Integration with HLFIR optimization passes
- HLFIR to FIR lowering optimization
- CPU-specific optimizations
- Performance benchmarking infrastructure
- Comparison with existing codegen backend

**Phase 3: GPU Support**
- SPIR-V dialect generation for GPU kernels
- Host-device memory management
- Kernel dispatch infrastructure
- GPU-specific optimizations

**Phase 4: Automatic Differentiation**
- Enzyme AD integration
- Gradient function generation
- AD validation against analytical gradients
- Combined CPU/GPU AD support

**14. Test-Driven Development Strategy**

The MLIR backend implementation will follow a comprehensive test-driven development approach:

**Test Infrastructure Requirements:**
- Unit tests for AST-to-HLFIR conversion
- Integration tests using `mlir-opt` and `mlir-translate`
- Round-trip validation tests (AST → HLFIR → FIR → LLVM IR → execution)
- HLFIR to FIR lowering validation tests
- Regression tests comparing outputs between backends
- Performance benchmarks for code generation

**Test Tools Integration:**
- `mlir-opt --verify-each` for MLIR validation
- `mlir-translate` for format conversion testing
- `llc` and `clang` for compilation pipeline testing
- Custom test harness for backend comparison

**Test Categories:**
1. **Syntax Tests**: Verify correct HLFIR and FIR dialect generation
2. **Semantic Tests**: Ensure behavior preservation across backends
3. **Lowering Tests**: Validate HLFIR to FIR conversion correctness
4. **Optimization Tests**: Validate HLFIR optimization and AD passes
5. **Integration Tests**: End-to-end compilation and execution
6. **Error Handling Tests**: Proper error reporting and recovery

**14. Future Extensions**

- Automatic pipeline script (`build.sh`) to orchestrate IR transforms.
- Custom `fir-opt` plugins for domain‑specific tiling/fusion.
- Embedding SPIR‑V blobs into the host binary for a single artifact.

---

*End of Updated Design Document*
