# Fortran Notebook Examples

This directory contains example Fortran notebook files (`.f`) that demonstrate the jupytext-like features of the `fortran` command.

## Running Notebooks

To execute a notebook and generate markdown output:

```bash
fortran --notebook simple_math.f -o simple_math_output.md
```

## Examples

### simple_math.f
Basic mathematical operations with output capture.

### arrays_loops.f
Array operations, loops, and reductions.

### control_flow.f
Control flow constructs including conditionals, loops, and case statements.

### plotting_demo.f
Integration with fortplotlib to embed plots as inline images.

## Notebook Syntax

- `! %% [markdown]` - Start a markdown cell
- `! %%` - Start a new code cell
- All code without markers is treated as part of the current cell
- Print statements are captured and displayed below each code cell
- `show()` calls from fortplotlib are converted to inline PNG images

## Output Format

The generated markdown includes:
- Formatted markdown cells
- Code cells with syntax highlighting
- Captured output from print statements
- Inline base64-encoded PNG images from plots