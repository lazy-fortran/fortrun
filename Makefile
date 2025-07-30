# Makefile for fortrun project

# Allow additional arguments to be passed
ARGS ?=

# FPM flags
FPM_FLAGS = --flag '-cpp'

.PHONY: all build test clean help coverage install debug example

# Default target
all: build

# Build the project
build:
	fpm build $(FPM_FLAGS) $(ARGS)

# Run all tests
test:
	fpm test $(FPM_FLAGS) $(ARGS)

# Generate coverage report
coverage:
	@echo "Cleaning previous coverage data..."
	@find . -name "*.gcda" -delete 2>/dev/null || true
	@rm -rf coverage/
	@echo "Building with coverage flags..."
	@fpm build --profile debug --flag '-cpp -fprofile-arcs -ftest-coverage -g'
	@echo "Running tests with coverage..."
	@fpm test --profile debug --flag '-cpp -fprofile-arcs -ftest-coverage -g' || true
	@echo "Generating coverage report with lcov..."
	@mkdir -p coverage
	# Capture coverage data
	@lcov --capture --directory build --output-file coverage/coverage.info \
		--rc branch_coverage=1 \
		--ignore-errors inconsistent,mismatch || true
	# Remove unwanted files from coverage
	@lcov --remove coverage/coverage.info \
		'build/dependencies/*' \
		'test/*' \
		'/usr/*' \
		--output-file coverage/coverage_filtered.info \
		--ignore-errors unused || true
	# Generate HTML report
	@genhtml coverage/coverage_filtered.info \
		--output-directory coverage \
		--branch-coverage \
		--legend || true
	# Generate XML report for codecov
	@python3 -m pip install --user lcov-cobertura >/dev/null 2>&1 || true
	@python3 -m lcov_cobertura coverage/coverage_filtered.info -o coverage/coverage.xml || true
	# Print summary
	@lcov --summary coverage/coverage_filtered.info 2>/dev/null || echo "No coverage data generated"
	@echo "Coverage report generated: coverage/index.html"
	@echo "XML report generated: coverage/coverage.xml"

# Check coverage threshold
coverage-check: coverage
	@echo "Checking coverage threshold..."
	@coverage=$$(grep -oP 'line-rate="\K[^"]+' coverage/coverage.xml | head -1); \
	coverage_percent=$$(echo "$$coverage * 100" | bc -l | cut -d. -f1); \
	echo "Total coverage: $${coverage_percent}%"; \
	if [ "$$coverage_percent" -lt "45" ]; then \
		echo "Coverage $${coverage_percent}% is below threshold of 45%"; \
		exit 1; \
	fi

# Install the project
install:
	chmod +x install.sh && ./install.sh $(ARGS)

# Run examples
example:
	fpm run fortrun -- example/basic/hello/hello.f90

# Clean build artifacts
clean:
	fpm clean --skip
	rm -rf coverage/
	find . -name '*.gcda' -delete 2>/dev/null || true
	find . -name '*.gcno' -delete 2>/dev/null || true

# Debug build
debug:
	fpm run fortrun -- $(ARGS) --debug

# Help target
help:
	@echo "Available targets:"
	@echo "  build          - Compile the project"
	@echo "  test           - Run all tests"
	@echo "  coverage       - Generate HTML and XML coverage reports"
	@echo "  coverage-check - Check if coverage meets threshold (45%)"
	@echo "  install        - Install the fortrun CLI"
	@echo "  example        - Run basic examples"
	@echo "  debug          - Run with debug output"
	@echo "  clean          - Clean build artifacts"
	@echo "  help           - Show this help message"
