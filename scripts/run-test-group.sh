#!/bin/bash
# Script to run specific test groups locally

set -e

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Function to run a test group
run_test_group() {
    local group=$1
    print_status "Running test group: $group"
    
    case $group in
        "core")
            # Lexer tests
            fpm test test_lexer_direct
            # Parser tests  
            fpm test test_parser_edge_cases
            fpm test test_parse_multi_decl
            fpm test test_param_body_order
            fpm test test_param_order_explicit
            fpm test test_frontend_parser_if_statement
            # Semantic tests
            fpm test test_semantic_simple
            fpm test test_semantic_context_components
            fpm test test_semantic_context_creation
            fpm test test_exact_semantic_context
            fpm test test_scope_manager_basic
            fpm test test_scope_pattern
            fpm test test_minimal_type_system
            fpm test test_function_type_creation
            fpm test test_minimal_ast_inferred_type
            ;;
        "utilities")
            fpm test test_logger_utils
            fpm test test_logger_utils_coverage
            fpm test test_string_utils
            fpm test test_string_utils_coverage
            fpm test test_system_utils_coverage
            fpm test test_system_utils_missing
            ;;
        "cache")
            fpm test test_cache
            fpm test test_cache_coverage
            fpm test test_cache_missing
            fpm test test_cache_lock
            fpm test test_cache_fallback
            ;;
        "runner")
            fpm test test_runner_paths
            fpm test test_runner_coverage
            fpm test test_runner_missing_lines
            fpm test test_runner_edge_cases
            fpm test test_testing_discovery
            fpm test test_testing_execution
            ;;
        "cli")
            fpm test test_cli_system
            fpm test test_cli_integration
            fpm test test_cli_comprehensive
            fpm test test_cli_json_options
            ;;
        "notebook")
            fpm test test_notebook_parser
            fpm test test_notebook_executor
            fpm test test_notebook_system
            fpm test test_notebook_integration
            ;;
        "fpm")
            fpm test test_fpm_generator
            fpm test test_fpm_module_cache
            fpm test test_fpm_version_generation
            ;;
        "module")
            fpm test test_module_scanner
            fpm test test_module_scanner_extended
            fpm test test_module_scanner_coverage
            ;;
        "misc")
            # Miscellaneous tests
            fpm test test_env_extend_operations
            fpm test test_minimal_alloc_bug
            fpm test test_nested_allocatable
            fpm test test_verbose
            fpm test test_different_directories
            fpm test test_error_handling
            fpm test test_file_isolation
            fpm test test_main_coverage
            ;;
        "frontend")
            export OMP_NUM_THREADS=4
            fpm test test_frontend_test_cases
            fpm test test_frontend_parser_derived_types
            fpm test test_frontend_parser_array_params
            fpm test test_frontend_parser_explicit_types
            fpm test test_frontend_full_explicit_params
            ;;
        "integration")
            export OMP_NUM_THREADS=4
            fpm test test_parse_and_codegen_arena
            fpm test test_runner_integration_coverage
            fpm test test_runner_comprehensive
            fpm test test_examples
            fpm test test_frontend_integration_trace
            fpm test test_critical_functionality
            ;;
        "quick")
            # Run a quick subset for rapid feedback
            fpm test test_string_utils
            fpm test test_logger_utils
            fpm test test_cache
            fpm test test_parser_edge_cases
            ;;
        "all")
            # Run all test groups
            for g in core utilities cache runner cli notebook fpm module misc frontend integration; do
                run_test_group "$g"
            done
            return
            ;;
        *)
            print_error "Unknown test group: $group"
            echo "Available groups: core, utilities, cache, runner, cli, notebook, fpm, module, misc, frontend, integration, quick, all"
            exit 1
            ;;
    esac
}

# Main script
if [ $# -eq 0 ]; then
    print_error "No test group specified"
    echo "Usage: $0 <test-group> [test-group2 ...]"
    echo "Available groups: core, utilities, cache, runner, cli, notebook, fpm, module, misc, frontend, integration, quick, all"
    exit 1
fi

# Build first
print_status "Building project..."
fpm build

# Run specified test groups
for group in "$@"; do
    run_test_group "$group"
done

print_status "All specified test groups completed!"