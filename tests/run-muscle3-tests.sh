#!/bin/bash
#######################################################################################################################
# MUSCLE3 Test Runner
# Runs only MUSCLE3-specific tests
#######################################################################################################################

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}MUSCLE3 Test Suite${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check if MUSCLE3 is installed
if ! python3 -c "import muscle3" 2>/dev/null; then
    echo -e "${RED}Error: MUSCLE3 is not installed${NC}"
    echo -e "${YELLOW}Install with: pip install iwrap[muscle3]${NC}"
    exit 1
fi

echo -e "${GREEN}✓ MUSCLE3 is installed${NC}"
echo ""

# Set PYTHONPATH
export PYTHONPATH=$(pwd):$PYTHONPATH

# Run MUSCLE3 tests
echo -e "${BLUE}Running MUSCLE3 tests...${NC}"
pytest tests/ -v -m "muscle3" --tb=short

if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✅ All MUSCLE3 tests PASSED!${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}❌ MUSCLE3 tests FAILED${NC}"
    exit 1
fi
