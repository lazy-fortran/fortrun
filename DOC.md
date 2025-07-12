# Documentation Structure Plan

This document outlines the planned structure for the `fortran` tool documentation using FORD.

## Reference: Fortplotlib Structure

Based on the analysis of `../fortplotlib`, we can see their approach:
- `fpm.toml` contains `[extra.ford]` section with minimal configuration
- `doc/index.md` serves as the main manual with `title: Manual`
- Examples are organized in `example/` with subdirectories

## Current State

### FORD Configuration
- ✅ FORD configuration integrated into `fpm.toml` under `[extra.ford]`
- ✅ Basic configuration includes `src_dir`, `output_dir`, `page_dir`, etc.

### Current Documentation Files
- `FORD.md` - Main documentation entry point (needs Getting Started section)
- `doc/index.md` - Currently minimal, needs to be developed as Manual
- `example/` - Well-organized with 9 subdirectories

### Example Directories
Current example folders that should be automatically included:
1. `advanced_inference/` - Type inference demonstrations
2. `calculator/` - Local module usage
3. `hello/` - Basic hello world
4. `interdependent/` - Complex interdependent modules
5. `notebook/` - Notebook-style execution examples
6. `plotting/` - External dependencies (pyplot-fortran)
7. `precision/` - Modern precision defaults
8. `preprocessor/` - .f file preprocessing
9. `type_inference/` - Type inference examples

## Proposed Documentation Structure

### 1. FORD.md - Main Entry Point
Should include:
```markdown
# Getting Started

## Installation
- Installation methods (manual build, future package managers)
- Prerequisites (FPM, Fortran compiler)

## Quick Start
- First example: `fortran hello.f90`
- Basic usage patterns
- Command-line options overview

## What is "Make Python Fortran again"?
- Philosophy and goals
- Comparison with Python workflow
- Zero-configuration approach
```

### 2. doc/Manual.md (rename index.md)
Should include:
```markdown
title: Manual
---

# Command Line Interface
- All CLI options detailed
- Configuration directories
- Registry system
- Caching mechanism

# Architecture
- How the tool works internally
- Module dependency resolution
- FPM integration
- Type inference system

# Advanced Usage
- Custom registry configuration
- Cache management
- Integration with existing projects
- Performance considerations

# Troubleshooting
- Common issues and solutions
- Debugging options
- Error messages guide
```

### 3. doc/Examples.md - Auto-generated
Should automatically include all example folders:
```markdown
title: Examples
---

# Examples

This section demonstrates various features of the `fortran` tool through practical examples.

## Basic Usage
- [Hello World](../example/hello/) - Simple program execution
- [Calculator](../example/calculator/) - Local module dependencies

## Advanced Features  
- [Interdependent Modules](../example/interdependent/) - Complex module relationships
- [Type Inference](../example/type_inference/) - Automatic type detection
- [Preprocessor](../example/preprocessor/) - .f file preprocessing

## Specialized Use Cases
- [Precision Handling](../example/precision/) - Modern precision defaults
- [Notebook Style](../example/notebook/) - Interactive-style execution
- [Plotting](../example/plotting/) - External library integration
- [Advanced Inference](../example/advanced_inference/) - Complex type scenarios

Each example includes source code, README documentation, and expected output.
```

## Implementation Plan

### Phase 1: Fix Manual Section
1. **Investigate why Manual section doesn't show up**
   - Check FORD configuration in `fpm.toml`
   - Verify `doc/index.md` title metadata
   - Test FORD generation

### Phase 2: Enhance Getting Started
2. **Expand FORD.md Getting Started section**
   - Add installation instructions
   - Include quick start examples
   - Explain core concepts

### Phase 3: Develop Manual
3. **Create comprehensive Manual**
   - Rename `doc/index.md` to `doc/Manual.md` if needed
   - Add detailed CLI documentation
   - Include architecture explanations

### Phase 4: Auto-generate Examples
4. **Create Examples section**
   - Add `doc/Examples.md` with automatic folder references
   - Link to each example subdirectory
   - Include brief descriptions from README files

### Phase 5: FORD Enhancement
5. **Optimize FORD configuration**
   - Ensure all sections appear correctly
   - Add proper navigation
   - Include API documentation links

## File Structure After Implementation

```
doc/
├── index.md -> Manual.md (or keep as index with proper title)
└── Examples.md (auto-references example/ folders)

example/ (existing structure, well-organized)
├── hello/
├── calculator/
├── precision/
├── interdependent/
├── plotting/
├── preprocessor/
├── notebook/
├── type_inference/
└── advanced_inference/
```

## Next Steps

1. Investigate current Manual section visibility issue
2. Implement Getting Started section in FORD.md
3. Enhance doc/index.md as comprehensive Manual
4. Create auto-generated Examples documentation
5. Test complete documentation generation with FORD