#!/bin/bash
# Helper script to test lazy fortran code snippets
# Usage: ./test_lazy.sh "code here"

if [ $# -eq 0 ]; then
    echo "Usage: $0 \"fortran_code\""
    echo "Example: $0 \"x = 42\""
    exit 1
fi

# Create temporary file
TEMP_FILE="/tmp/lazy_test_$$.f"

# Write code to temp file
echo "$1" > "$TEMP_FILE"

# Run fortran compiler
fpm run fortran -- "$TEMP_FILE" --preprocess

# Clean up
rm -f "$TEMP_FILE"