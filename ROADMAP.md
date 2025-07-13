# ROADMAP.md

## Project Vision

**Make Python Fortran again.** The `fortran` CLI tool aims to make Fortran development as seamless as Python, with automatic dependency resolution, transparent compilation, and eventually a simplified syntax that compiles to standard Fortran.

## Completed Features ✅

- **Basic CLI, OS Cache Management, Local Dependencies, FPM Integration**
- **Module Registry with Smart Resolution**
- **Smart Caching System with 2-4x Performance Improvements**
- **Modern Defaults (implicit none, real*8)**
- **Simplified Fortran Preprocessor (.f files)**
- **Type Inference (Basic & Advanced)**
- **Notebook Support with Figure/Plot Integration**

## Current Goal: Clean Parser and Code Generator 🚧

### Immediate Objective
Replace the ad-hoc line-based preprocessor with a proper **lexer/parser/AST/codegen** architecture:
- Parse Simple Fortran (.f) syntax
- Generate standard Fortran 95 code
- Maintain flexibility for future Fortran 2003+ support
- Prepare foundation for compiler IR integration

### AST Architecture Progress
- ✅ **Lexer**: Complete tokenization with JSON serialization
- ✅ **AST Definition**: Comprehensive node types with visitor pattern
- 🚧 **Parser**: Basic expression parsing complete, need statement/function parsing
- 🚧 **Code Generator**: Basic implementation, needs modern defaults and formatting
- 🚧 **Integration**: AST preprocessor partially working, needs full feature support

### Near-term Targets
1. **Complete Parser Implementation**
   - Statement parsing (assignments, prints)
   - Function/subroutine definitions
   - Implicit program wrapping
   - Error recovery and reporting

2. **Enhance Code Generation**
   - Apply modern Fortran defaults
   - Proper indentation and formatting
   - Generate contains statements
   - Support all Simple Fortran features

3. **Full AST Preprocessor**
   - Support arrays and derived types
   - Advanced type inference integration
   - Preserve comments and documentation
   - Symbol table and scope management

## Modern Fortran Ecosystem Integration 🌐

### FPM Registry Integration
- **Official Registry Sync**: Direct integration with fortran-lang/fpm-registry
- **Module Database**: Extract and index all modules from registered packages
- **Fast Local Resolution**: Build efficient module→package lookup database
- **Automatic Updates**: Periodic sync with official registry

### Module Resolution Architecture
1. **Registry Crawler**: Fetch package metadata from FPM registry
2. **Module Extractor**: Parse packages to build module inventory
3. **Local Database**: SQLite/binary format for sub-millisecond lookups
4. **Smart Caching**: Incremental updates, not full re-scans
5. **Fallback Strategy**: Current TOML registry for custom/private packages

### Performance Targets
- **Module Resolution**: < 1ms for module→package lookup
- **Database Size**: < 10MB for entire FPM ecosystem
- **Update Time**: < 30s for incremental registry sync
- **Zero Network Calls**: During normal compilation

### Implementation Plan
- [ ] Design efficient module database schema
- [ ] Build FPM registry crawler and parser
- [ ] Create module extraction tool
- [ ] Implement fast local lookup engine
- [ ] Add registry sync command (`fortran --sync-registry`)
- [ ] Integrate with existing resolution pipeline

## Long-term Vision

### Multi-Standard Support
- **Phase 1**: Fortran 95 target (current)
- **Phase 2**: Fortran 2003 features (OOP, parameterized types)
- **Phase 3**: Fortran 2008+ (coarrays, submodules)
- **Flexibility**: Pluggable code generation backends

### Compiler Integration
- **Direct IR Generation**: Skip source-to-source transformation
- **LLVM Backend**: Generate LLVM IR directly from AST
- **GFortran Integration**: Interface with GCC's intermediate representation
- **Benefits**: Faster compilation, better optimization opportunities

### Advanced Language Features
- **Multiple Dispatch**: Julia-like generic programming
- **Type Classes**: Haskell-inspired abstractions
- **Metaprogramming**: Compile-time code generation
- **Interop**: Seamless C/C++/Python integration

## Implementation Strategy

### Current Focus (AST Branch)
1. **Complete the Parser**: Full Simple Fortran support
2. **Production-ready Codegen**: All features, proper formatting
3. **Replace Legacy Preprocessor**: Full migration to AST-based approach
4. **Comprehensive Testing**: All examples must work

### Architecture Principles
- **Modular Design**: Separate lexer, parser, AST, codegen phases
- **Clean Interfaces**: Well-defined boundaries between components
- **Extensibility**: Easy to add new language features
- **Performance**: Fast parsing and code generation
- **Debugging**: Preserve source locations, generate source maps

### Quality Metrics
- All existing tests pass
- All examples work without modification
- Performance equal or better than current preprocessor
- Clean, maintainable codebase
- Foundation ready for IR generation

## Success Criteria

### Short-term (AST Implementation)
- ✅ Working lexer with comprehensive tokenization
- ✅ Complete AST node definitions
- 🚧 Full parser for Simple Fortran
- 🚧 Code generator producing valid Fortran 95
- ⬜ All regression tests passing
- ⬜ Legacy preprocessor retired

### Medium-term (Compiler Integration)
- ⬜ Direct LLVM IR generation prototype
- ⬜ GFortran IR interface exploration
- ⬜ Performance benchmarks vs traditional compilation
- ⬜ Source map generation for debugging

### Long-term (Language Evolution)
- ⬜ Multiple dispatch implementation
- ⬜ Advanced type system features
- ⬜ Metaprogramming capabilities
- ⬜ Full Python/Julia-like developer experience

## Timeline

- **Q1 2024**: Complete AST-based preprocessor
- **Q2 2024**: FPM Registry integration & Module database
- **Q3 2024**: Fortran 2003 feature support
- **Q4 2024**: LLVM IR generation prototype
- **2025**: Multiple dispatch & Advanced type system

The goal remains: **Make Python Fortran again** - combining Python's ease of use with Fortran's performance and numerical computing strengths, while being a first-class citizen in the Modern Fortran ecosystem.