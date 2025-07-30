#!/bin/bash
set -e

echo "=== Fortrun Coverage Report with lcov ==="
echo ""

# Clean previous coverage data
echo "Cleaning previous coverage data..."
find . -name "*.gcda" -delete 2>/dev/null || true
find . -name "*.gcno" -delete 2>/dev/null || true
rm -rf coverage/

# Build with coverage flags
echo "Building with coverage flags..."
fpm build --flag '-cpp -fprofile-arcs -ftest-coverage -g'

# Run tests
echo "Running tests..."
fpm test --flag '-cpp -fprofile-arcs -ftest-coverage -g' || true

# Create coverage directory
mkdir -p coverage

# Capture coverage data
echo "Capturing coverage data..."
lcov --capture \
     --directory build \
     --output-file coverage/coverage.info \
     --rc lcov_branch_coverage=1 \
     --quiet

# Remove unwanted files from coverage
echo "Filtering coverage data..."
lcov --remove coverage/coverage.info \
     '*/build/dependencies/*' \
     '*/test/*' \
     '*/example/*' \
     '*/packages/*' \
     '/usr/*' \
     --output-file coverage/coverage_filtered.info \
     --rc lcov_branch_coverage=1 \
     --quiet

# Generate HTML report
echo "Generating HTML report..."
genhtml coverage/coverage_filtered.info \
        --output-directory coverage \
        --branch-coverage \
        --function-coverage \
        --title "Fortrun Coverage Report" \
        --legend \
        --show-details \
        --quiet

# Generate XML report for codecov
echo "Generating XML report for codecov..."
lcov_cobertura coverage/coverage_filtered.info -o coverage/coverage.xml

# Print summary
echo ""
echo "=== Coverage Summary ==="
lcov --summary coverage/coverage_filtered.info --rc lcov_branch_coverage=1

echo ""
echo "Coverage report generated:"
echo "  HTML: coverage/index.html"
echo "  XML:  coverage/coverage.xml"
