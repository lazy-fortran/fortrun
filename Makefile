# Makefile for fortran CLI project
# Inspired by ../fortplot/Makefile

# Allow additional arguments to be passed
ARGS ?=

# FPM flags
FPM_FLAGS = 

.PHONY: all build test clean help coverage coverage-html install debug example

# Default target
all: build

# Build the project
build:
	fpm build $(FPM_FLAGS) $(ARGS)

# Run all tests (using built-in parallel test runner)
test:
	OMP_NUM_THREADS=24 fpm run fortran -- --test $(ARGS)

# Run specific test with fpm test (for debugging)
test-single:
	fpm test $(ARGS)

# Run CLI tests
test-cli:
	OMP_NUM_THREADS=24 fpm run fortran -- --test --filter cli $(ARGS)

# Run frontend tests  
test-frontend:
	OMP_NUM_THREADS=24 fpm run fortran -- --test --filter frontend $(ARGS)

# Run cache tests
test-cache:
	OMP_NUM_THREADS=24 fpm run fortran -- --test --filter cache $(ARGS)

# Install the project
install:
	chmod +x install.sh && ./install.sh $(ARGS)

# Run examples
example:
	fpm run fortran -- example/hello.f
	fpm run fortran -- example/basic/simple.f

# Generate comprehensive coverage report with ALL tests
coverage:
	@echo "Cleaning old coverage data..."
	find . -name '*.gcda' -delete
	@echo "Building with coverage flags..."
	fpm build --flag '-fprofile-arcs -ftest-coverage'
	@echo "Running ALL tests with coverage (using built-in parallel runner)..."
	OMP_NUM_THREADS=24 fpm run fortran -- --test --flag '-fprofile-arcs -ftest-coverage' || true
	@echo "Also running fpm test for any additional coverage..."
	fpm test --flag '-fprofile-arcs -ftest-coverage' || true
	@echo "Generating coverage report..."
	gcovr --root . \
		--exclude 'build/*' \
		--exclude 'test/*' \
		--exclude 'example/*' \
		--exclude 'app/test_*.f90' \
		--exclude 'draft/*' \
		--txt -o coverage.txt \
		--print-summary
	@echo "Coverage report generated: coverage.txt"
	@cat coverage.txt

# Generate HTML coverage report  
coverage-html:
	@echo "Cleaning old coverage data..."
	find . -name '*.gcda' -delete
	@echo "Building with coverage flags..."
	fpm build --flag '-fprofile-arcs -ftest-coverage'
	@echo "Running ALL tests with coverage (using built-in parallel runner)..."
	OMP_NUM_THREADS=24 fpm run fortran -- --test --flag '-fprofile-arcs -ftest-coverage' || true
	@echo "Also running fpm test for any additional coverage..."
	fpm test --flag '-fprofile-arcs -ftest-coverage' || true  
	@echo "Generating HTML coverage report..."
	mkdir -p coverage
	gcovr --root . \
		--exclude 'build/*' \
		--exclude 'test/*' \
		--exclude 'example/*' \
		--exclude 'app/test_*.f90' \
		--exclude 'draft/*' \
		--html-details -o coverage/index.html \
		--print-summary
	@echo "HTML coverage report generated: coverage/index.html"

# Clean build artifacts  
clean:
	fpm clean --skip
	rm -rf coverage/
	find . -name '*.gcda' -delete
	find . -name '*.gcno' -delete
	rm -f coverage.txt coverage.xml

# Debug build
debug:
	fpm run fortran -- $(ARGS) --debug

# Clear cache
clear-cache:
	fpm run fortran -- --clear-cache

# Build and run with release optimizations
release:
	fpm build --profile release $(ARGS)

# Help target
help:
	@echo "Available targets:"
	@echo "  build            - Compile the project"  
	@echo "  test             - Run all tests (parallel)"
	@echo "  test-cli         - Run CLI tests only"
	@echo "  test-frontend    - Run frontend tests only" 
	@echo "  test-cache       - Run cache tests only"
	@echo "  test-single      - Run single test with fpm test"
	@echo "  coverage         - Generate text coverage report (runs ALL tests)"
	@echo "  coverage-html    - Generate HTML coverage report (runs ALL tests)"
	@echo "  install          - Install the fortran CLI"
	@echo "  example          - Run basic examples"
	@echo "  debug            - Run with debug output"
	@echo "  clear-cache      - Clear fortran CLI cache"
	@echo "  clean            - Clean build artifacts and coverage data"
	@echo "  release          - Build with optimizations"  
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Pass additional arguments using ARGS variable:"
	@echo "  make test ARGS=\"-v\""
	@echo "  make test-cli ARGS=\"--filter integration\""
	@echo "  make debug ARGS=\"example/hello.f -vv\""