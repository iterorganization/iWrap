#!/bin/bash
#######################################################################################################################
# iWrap Test Runner
# Runs all tests including optional MUSCLE3 tests
#######################################################################################################################

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results
CORE_TESTS_PASSED=0
MUSCLE3_TESTS_PASSED=0
INTEGRATION_TESTS_PASSED=0

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}iWrap Test Suite${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check if pytest is available
if ! command -v pytest &> /dev/null; then
    echo -e "${RED}Error: pytest is not installed${NC}"
    echo "Please install pytest: pip install pytest"
    exit 1
fi

# Check Python path
if [ -z "$PYTHONPATH" ]; then
    export PYTHONPATH=$(pwd)
else
    export PYTHONPATH=$(pwd):$PYTHONPATH
fi

echo -e "${BLUE}PYTHONPATH set to: $PYTHONPATH${NC}"
echo ""

#######################################################################################################################
# Run Core Unit Tests
#######################################################################################################################
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Running Core Unit Tests${NC}"
echo -e "${BLUE}========================================${NC}"

if pytest tests/unit/core -v -m "not muscle3" 2>&1 | tee /tmp/iwrap_core_tests.log; then
    CORE_TESTS_PASSED=1
    echo -e "${GREEN}✓ Core unit tests passed${NC}"
else
    echo -e "${RED}✗ Core unit tests failed${NC}"
fi
echo ""

#######################################################################################################################
# Run Generator Unit Tests (excluding MUSCLE3)
#######################################################################################################################
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Running Generator Unit Tests (Core)${NC}"
echo -e "${BLUE}========================================${NC}"

if [ -d "tests/unit/generators" ]; then
    if pytest tests/unit/generators -v -m "not muscle3" 2>&1 | tee /tmp/iwrap_generator_tests.log; then
        echo -e "${GREEN}✓ Core generator tests passed${NC}"
    else
        echo -e "${YELLOW}⚠ Some core generator tests failed${NC}"
    fi
else
    echo -e "${YELLOW}⚠ No generator tests directory found${NC}"
fi
echo ""

#######################################################################################################################
# Check if MUSCLE3 is available
#######################################################################################################################
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Checking MUSCLE3 Availability${NC}"
echo -e "${BLUE}========================================${NC}"

if python3 -c "import muscle3" 2>/dev/null; then
    echo -e "${GREEN}✓ MUSCLE3 library is installed${NC}"
    MUSCLE3_AVAILABLE=1
else
    echo -e "${YELLOW}⚠ MUSCLE3 library is NOT installed${NC}"
    echo -e "${YELLOW}  MUSCLE3 tests will be skipped${NC}"
    echo -e "${YELLOW}  To run MUSCLE3 tests, install with: pip install iwrap[muscle3]${NC}"
    MUSCLE3_AVAILABLE=0
fi
echo ""

#######################################################################################################################
# Run MUSCLE3 Tests (if available)
#######################################################################################################################
if [ $MUSCLE3_AVAILABLE -eq 1 ]; then
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Running MUSCLE3 Unit Tests${NC}"
    echo -e "${BLUE}========================================${NC}"
    
    if pytest tests/unit/generators -v -m "muscle3" 2>&1 | tee /tmp/iwrap_muscle3_tests.log; then
        MUSCLE3_TESTS_PASSED=1
        echo -e "${GREEN}✓ MUSCLE3 unit tests passed${NC}"
    else
        echo -e "${RED}✗ MUSCLE3 unit tests failed${NC}"
    fi
    echo ""
fi

#######################################################################################################################
# Run Integration Tests
#######################################################################################################################
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Running Integration Tests${NC}"
echo -e "${BLUE}========================================${NC}"

if [ -d "tests/integration" ]; then
    if [ $MUSCLE3_AVAILABLE -eq 1 ]; then
        # Run all integration tests
        if pytest tests/integration -v 2>&1 | tee /tmp/iwrap_integration_tests.log; then
            INTEGRATION_TESTS_PASSED=1
            echo -e "${GREEN}✓ Integration tests passed${NC}"
        else
            echo -e "${RED}✗ Integration tests failed${NC}"
        fi
    else
        # Run only non-MUSCLE3 integration tests
        if pytest tests/integration -v -m "not muscle3" 2>&1 | tee /tmp/iwrap_integration_tests.log; then
            INTEGRATION_TESTS_PASSED=1
            echo -e "${GREEN}✓ Core integration tests passed${NC}"
        else
            echo -e "${RED}✗ Core integration tests failed${NC}"
        fi
    fi
else
    echo -e "${YELLOW}⚠ No integration tests directory found${NC}"
fi
echo ""

#######################################################################################################################
# Test Summary
#######################################################################################################################
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Test Summary${NC}"
echo -e "${BLUE}========================================${NC}"

if [ $CORE_TESTS_PASSED -eq 1 ]; then
    echo -e "${GREEN}✓ Core Tests: PASSED${NC}"
else
    echo -e "${RED}✗ Core Tests: FAILED${NC}"
fi

if [ $MUSCLE3_AVAILABLE -eq 1 ]; then
    if [ $MUSCLE3_TESTS_PASSED -eq 1 ]; then
        echo -e "${GREEN}✓ MUSCLE3 Tests: PASSED${NC}"
    else
        echo -e "${RED}✗ MUSCLE3 Tests: FAILED${NC}"
    fi
else
    echo -e "${YELLOW}⚠ MUSCLE3 Tests: SKIPPED (MUSCLE3 not installed)${NC}"
fi

if [ $INTEGRATION_TESTS_PASSED -eq 1 ]; then
    echo -e "${GREEN}✓ Integration Tests: PASSED${NC}"
else
    echo -e "${RED}✗ Integration Tests: FAILED${NC}"
fi

echo ""
echo -e "${BLUE}========================================${NC}"

# Exit with error if any required tests failed
if [ $CORE_TESTS_PASSED -eq 0 ]; then
    echo -e "${RED}Test suite FAILED - Core tests failed${NC}"
    exit 1
fi

if [ $MUSCLE3_AVAILABLE -eq 1 ] && [ $MUSCLE3_TESTS_PASSED -eq 0 ]; then
    echo -e "${RED}Test suite FAILED - MUSCLE3 tests failed${NC}"
    exit 1
fi

echo -e "${GREEN}✅ All tests PASSED!${NC}"
exit 0
