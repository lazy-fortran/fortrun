#!/bin/bash
# Script to generate coverage report locally with parallel test execution

set -e

# Color codes
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${GREEN}[INFO]${NC} Cleaning old coverage data..."
find . -name '*.gcda' -delete 2>/dev/null || true
find . -name '*.gcno' -delete 2>/dev/null || true
rm -rf coverage-reports/
mkdir -p coverage-reports

echo -e "${GREEN}[INFO]${NC} Building with coverage flags..."
fpm build --flag '-cpp -fprofile-arcs -ftest-coverage'

# Function to run test group and collect coverage
run_test_group_coverage() {
    local group=$1
    echo -e "${GREEN}[INFO]${NC} Running $group tests with coverage..."

    # Run the test group
    ./scripts/run-test-group.sh "$group" || true

    # Generate JSON report for this group
    gcovr --root . \
        --exclude 'build/*' \
        --exclude 'test/*' \
        --exclude 'example/*' \
        --exclude 'app/test_*.f90' \
        --exclude 'draft/*' \
        --json "coverage-reports/coverage-${group}.json"

    echo -e "${GREEN}[INFO]${NC} Coverage data collected for $group"
}

# Run test groups in parallel (if GNU parallel is available)
if command -v parallel &> /dev/null; then
    echo -e "${GREEN}[INFO]${NC} Running tests in parallel with GNU parallel..."
    export -f run_test_group_coverage
    parallel -j 4 run_test_group_coverage ::: core utilities cache runner cli notebook fpm module
else
    echo -e "${YELLOW}[WARN]${NC} GNU parallel not found, running tests sequentially..."
    for group in core utilities cache runner cli notebook fpm module; do
        run_test_group_coverage "$group"
    done
fi

# Run heavy tests sequentially
echo -e "${GREEN}[INFO]${NC} Running heavy tests..."
run_test_group_coverage frontend
run_test_group_coverage integration

echo -e "${GREEN}[INFO]${NC} Merging coverage reports..."

# Merge all JSON reports
gcovr --add-tracefile "coverage-reports/coverage-*.json" \
    --root . \
    --exclude 'build/*' \
    --exclude 'test/*' \
    --exclude 'example/*' \
    --exclude 'app/test_*.f90' \
    --exclude 'draft/*' \
    --xml -o coverage.xml \
    --html-details coverage-reports/index.html \
    --txt -o coverage.txt \
    --print-summary

echo -e "${GREEN}[INFO]${NC} Coverage reports generated:"
echo "  - XML: coverage.xml (for CI/CD)"
echo "  - HTML: coverage-reports/index.html"
echo "  - Text: coverage.txt"
echo ""
cat coverage.txt
