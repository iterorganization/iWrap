#!/bin/bash
#######################################################################################################################
# Master Test Orchestrator for iWrap
# Runs both pytest-based tests (unit/integration) and IMAS integration tests
#######################################################################################################################

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Default configuration
RUN_UNIT_TESTS=1
RUN_INTEGRATION_TESTS=1
RUN_IMAS_TESTS=0  # IMAS tests are optional and slow
RUN_MUSCLE3_TESTS=1  # Run if MUSCLE3 is available
VERBOSE=0
FAIL_FAST=0

# Results tracking
UNIT_TESTS_RESULT=0
INTEGRATION_TESTS_RESULT=0
IMAS_TESTS_RESULT=0
MUSCLE3_TESTS_RESULT=0

#######################################################################################################################
# Help Function
#######################################################################################################################
print_help() {
    cat << EOF
${BLUE}iWrap Master Test Suite${NC}

Usage: $0 [OPTIONS]

Options:
    --unit-only         Run only unit tests
    --integration-only  Run only integration tests
    --imas              Run IMAS integration tests (slow)
    --muscle3-only      Run only MUSCLE3 tests
    --all               Run all tests including IMAS
    --skip-muscle3      Skip MUSCLE3 tests even if available
    --fail-fast         Stop on first failure
    -v, --verbose       Verbose output
    -h, --help          Show this help message

Test Types:
    Unit Tests:         Fast, isolated component tests (pytest)
    Integration Tests:  Cross-component workflow tests (pytest)
    MUSCLE3 Tests:      Tests requiring MUSCLE3 library (pytest)
    IMAS Tests:         Full IMAS integration tests (legacy test suite)

Examples:
    $0                          # Run unit + integration tests
    $0 --all                    # Run all tests including IMAS
    $0 --unit-only              # Run only unit tests
    $0 --muscle3-only           # Run only MUSCLE3 tests
    $0 --imas                   # Include IMAS tests
    $0 --skip-muscle3           # Skip MUSCLE3 tests

EOF
}

#######################################################################################################################
# Parse Command Line Arguments
#######################################################################################################################
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --unit-only)
                RUN_INTEGRATION_TESTS=0
                RUN_IMAS_TESTS=0
                shift
                ;;
            --integration-only)
                RUN_UNIT_TESTS=0
                RUN_IMAS_TESTS=0
                shift
                ;;
            --muscle3-only)
                RUN_UNIT_TESTS=0
                RUN_INTEGRATION_TESTS=0
                RUN_IMAS_TESTS=0
                shift
                ;;
            --imas)
                RUN_IMAS_TESTS=1
                shift
                ;;
            --all)
                RUN_UNIT_TESTS=1
                RUN_INTEGRATION_TESTS=1
                RUN_IMAS_TESTS=1
                shift
                ;;
            --skip-muscle3)
                RUN_MUSCLE3_TESTS=0
                shift
                ;;
            --fail-fast)
                FAIL_FAST=1
                shift
                ;;
            -v|--verbose)
                VERBOSE=1
                shift
                ;;
            -h|--help)
                print_help
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                print_help
                exit 1
                ;;
        esac
    done
}

#######################################################################################################################
# Environment Setup
#######################################################################################################################
setup_environment() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Environment Setup${NC}"
    echo -e "${BLUE}========================================${NC}"
    
    # Set PYTHONPATH
    export PYTHONPATH=$(pwd):${PYTHONPATH}
    echo -e "PYTHONPATH: ${PYTHONPATH}"
    
    # Check pytest
    if ! command -v pytest &> /dev/null; then
        echo -e "${RED}Error: pytest is not installed${NC}"
        echo "Install with: pip install pytest"
        exit 1
    fi
    echo -e "${GREEN}✓ pytest is available${NC}"
    
    # Check MUSCLE3
    if python3 -c "import muscle3" 2>/dev/null; then
        echo -e "${GREEN}✓ MUSCLE3 is available${NC}"
        MUSCLE3_AVAILABLE=1
    else
        echo -e "${YELLOW}⚠ MUSCLE3 is not available (tests will be skipped)${NC}"
        MUSCLE3_AVAILABLE=0
        if [ $RUN_MUSCLE3_TESTS -eq 1 ]; then
            RUN_MUSCLE3_TESTS=0
        fi
    fi
    
    echo ""
}

#######################################################################################################################
# Run Unit Tests
#######################################################################################################################
run_unit_tests() {
    if [ $RUN_UNIT_TESTS -eq 0 ]; then
        return 0
    fi
    
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Running Unit Tests${NC}"
    echo -e "${BLUE}========================================${NC}"
    
    local pytest_args="-v -m unit"
    if [ $VERBOSE -eq 1 ]; then
        pytest_args="$pytest_args -vv"
    fi
    
    if [ $FAIL_FAST -eq 1 ]; then
        pytest_args="$pytest_args -x"
    fi
    
    # Run unit tests, excluding MUSCLE3 if not available
    if [ $MUSCLE3_AVAILABLE -eq 0 ]; then
        pytest_args="$pytest_args and not muscle3"
    fi
    
    if pytest tests/unit/ $pytest_args 2>&1 | tee /tmp/iwrap_unit_tests.log; then
        echo -e "${GREEN}✓ Unit tests PASSED${NC}"
        UNIT_TESTS_RESULT=0
    else
        echo -e "${RED}✗ Unit tests FAILED${NC}"
        UNIT_TESTS_RESULT=1
        if [ $FAIL_FAST -eq 1 ]; then
            return 1
        fi
    fi
    echo ""
    
    return $UNIT_TESTS_RESULT
}

#######################################################################################################################
# Run Integration Tests
#######################################################################################################################
run_integration_tests() {
    if [ $RUN_INTEGRATION_TESTS -eq 0 ]; then
        return 0
    fi
    
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Running Integration Tests${NC}"
    echo -e "${BLUE}========================================${NC}"
    
    local pytest_args="-v -m integration"
    if [ $VERBOSE -eq 1 ]; then
        pytest_args="$pytest_args -vv"
    fi
    
    if [ $FAIL_FAST -eq 1 ]; then
        pytest_args="$pytest_args -x"
    fi
    
    # Run integration tests, excluding MUSCLE3 if not available
    if [ $MUSCLE3_AVAILABLE -eq 0 ]; then
        pytest_args="$pytest_args and not muscle3"
    fi
    
    if pytest tests/integration/ $pytest_args 2>&1 | tee /tmp/iwrap_integration_tests.log; then
        echo -e "${GREEN}✓ Integration tests PASSED${NC}"
        INTEGRATION_TESTS_RESULT=0
    else
        echo -e "${RED}✗ Integration tests FAILED${NC}"
        INTEGRATION_TESTS_RESULT=1
        if [ $FAIL_FAST -eq 1 ]; then
            return 1
        fi
    fi
    echo ""
    
    return $INTEGRATION_TESTS_RESULT
}

#######################################################################################################################
# Run MUSCLE3 Tests
#######################################################################################################################
run_muscle3_tests() {
    if [ $RUN_MUSCLE3_TESTS -eq 0 ] || [ $MUSCLE3_AVAILABLE -eq 0 ]; then
        return 0
    fi
    
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Running MUSCLE3 Tests${NC}"
    echo -e "${BLUE}========================================${NC}"
    
    local pytest_args="-v -m muscle3"
    if [ $VERBOSE -eq 1 ]; then
        pytest_args="$pytest_args -vv"
    fi
    
    if [ $FAIL_FAST -eq 1 ]; then
        pytest_args="$pytest_args -x"
    fi
    
    if pytest tests/ $pytest_args 2>&1 | tee /tmp/iwrap_muscle3_tests.log; then
        echo -e "${GREEN}✓ MUSCLE3 tests PASSED${NC}"
        MUSCLE3_TESTS_RESULT=0
    else
        echo -e "${RED}✗ MUSCLE3 tests FAILED${NC}"
        MUSCLE3_TESTS_RESULT=1
        if [ $FAIL_FAST -eq 1 ]; then
            return 1
        fi
    fi
    echo ""
    
    return $MUSCLE3_TESTS_RESULT
}

#######################################################################################################################
# Run IMAS Tests
#######################################################################################################################
run_imas_tests() {
    if [ $RUN_IMAS_TESTS -eq 0 ]; then
        return 0
    fi
    
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Running IMAS Integration Tests${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo -e "${YELLOW}Note: IMAS tests are slow and require IMAS environment${NC}"
    echo ""
    
    if [ -f "./tests/run-tests.sh" ]; then
        if ./tests/run-tests.sh 2>&1 | tee /tmp/iwrap_imas_tests.log; then
            echo -e "${GREEN}✓ IMAS tests PASSED${NC}"
            IMAS_TESTS_RESULT=0
        else
            echo -e "${RED}✗ IMAS tests FAILED${NC}"
            IMAS_TESTS_RESULT=1
        fi
    else
        echo -e "${YELLOW}⚠ IMAS test script not found${NC}"
        IMAS_TESTS_RESULT=0
    fi
    echo ""
    
    return $IMAS_TESTS_RESULT
}

#######################################################################################################################
# Print Summary
#######################################################################################################################
print_summary() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Test Summary${NC}"
    echo -e "${BLUE}========================================${NC}"
    
    local total_failed=0
    
    if [ $RUN_UNIT_TESTS -eq 1 ]; then
        if [ $UNIT_TESTS_RESULT -eq 0 ]; then
            echo -e "${GREEN}✓ Unit Tests: PASSED${NC}"
        else
            echo -e "${RED}✗ Unit Tests: FAILED${NC}"
            total_failed=$((total_failed + 1))
        fi
    fi
    
    if [ $RUN_INTEGRATION_TESTS -eq 1 ]; then
        if [ $INTEGRATION_TESTS_RESULT -eq 0 ]; then
            echo -e "${GREEN}✓ Integration Tests: PASSED${NC}"
        else
            echo -e "${RED}✗ Integration Tests: FAILED${NC}"
            total_failed=$((total_failed + 1))
        fi
    fi
    
    if [ $MUSCLE3_AVAILABLE -eq 1 ] && [ $RUN_MUSCLE3_TESTS -eq 1 ]; then
        if [ $MUSCLE3_TESTS_RESULT -eq 0 ]; then
            echo -e "${GREEN}✓ MUSCLE3 Tests: PASSED${NC}"
        else
            echo -e "${RED}✗ MUSCLE3 Tests: FAILED${NC}"
            total_failed=$((total_failed + 1))
        fi
    elif [ $RUN_MUSCLE3_TESTS -eq 1 ]; then
        echo -e "${YELLOW}⚠ MUSCLE3 Tests: SKIPPED (not installed)${NC}"
    fi
    
    if [ $RUN_IMAS_TESTS -eq 1 ]; then
        if [ $IMAS_TESTS_RESULT -eq 0 ]; then
            echo -e "${GREEN}✓ IMAS Tests: PASSED${NC}"
        else
            echo -e "${RED}✗ IMAS Tests: FAILED${NC}"
            total_failed=$((total_failed + 1))
        fi
    fi
    
    echo -e "${BLUE}========================================${NC}"
    
    if [ $total_failed -eq 0 ]; then
        echo -e "${GREEN}✅ All tests PASSED!${NC}"
        return 0
    else
        echo -e "${RED}❌ $total_failed test suite(s) FAILED${NC}"
        return 1
    fi
}

#######################################################################################################################
# Main Execution
#######################################################################################################################
main() {
    echo -e "${CYAN}"
    echo "╔════════════════════════════════════════╗"
    echo "║     iWrap Master Test Suite            ║"
    echo "╚════════════════════════════════════════╝"
    echo -e "${NC}"
    echo ""
    
    parse_args "$@"
    setup_environment
    
    # Run tests
    run_unit_tests
    run_integration_tests
    run_muscle3_tests
    run_imas_tests
    
    # Print summary and exit
    print_summary
    exit $?
}

# Run main
main "$@"
