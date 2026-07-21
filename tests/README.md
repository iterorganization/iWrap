# iWrap Test Suite

This directory contains the comprehensive test suite for iWrap, including unit tests, integration tests, and IMAS integration tests.

## Test Structure

```
tests/
├── conftest.py                    # Pytest configuration and shared fixtures
├── pytest.ini                     # Pytest markers and settings
│
├── run-all-tests-master.sh       # Master orchestrator for all tests
├── run-all-tests.sh              # Comprehensive pytest-based tests
├── run-muscle3-tests.sh          # MUSCLE3-specific tests only
├── run-tests.sh                  # Legacy IMAS integration tests
│
├── unit/                         # Fast, isolated component tests
│   ├── core/                     # Core functionality tests
│   └── generators/               # Generator-specific tests
│       ├── test_muscle3_python.py
│       ├── test_muscle3_cpp.py
│       └── test_muscle3_fortran.py
│
├── integration/                  # Cross-component workflow tests
│   └── test_muscle3_integration.py
│
├── muscle3/                      # MUSCLE3-specific test cases
│   ├── actors/
│   ├── wrapped_codes/
│   ├── macro/
│   ├── workflow/
│   └── code_parameters/
│
└── test_cases/                   # IMAS integration test cases
    └── ...
```

## Running Tests

### Quick Start

Run all pytest-based tests (unit + integration):
```bash
./tests/run-all-tests-master.sh
```

### Test Runners

#### Master Orchestrator (Recommended)
The master orchestrator provides fine-grained control over which test suites to run:

```bash
# Run all tests including IMAS (slow)
./tests/run-all-tests-master.sh --all

# Run only unit tests (fast)
./tests/run-all-tests-master.sh --unit-only

# Run only integration tests
./tests/run-all-tests-master.sh --integration-only

# Run only MUSCLE3 tests
./tests/run-all-tests-master.sh --muscle3-only

# Include IMAS tests
./tests/run-all-tests-master.sh --imas

# Skip MUSCLE3 tests
./tests/run-all-tests-master.sh --skip-muscle3

# Verbose output
./tests/run-all-tests-master.sh -v

# Stop on first failure
./tests/run-all-tests-master.sh --fail-fast
```

#### Comprehensive Pytest Tests
Run all pytest-based tests with detailed reporting:
```bash
./tests/run-all-tests.sh
```

This script:
- Checks for MUSCLE3 availability
- Runs core unit tests
- Runs generator unit tests
- Runs MUSCLE3 tests (if available)
- Runs integration tests
- Provides color-coded summary

#### MUSCLE3-Only Tests
Run only MUSCLE3 tests (requires MUSCLE3 installation):
```bash
./tests/run-muscle3-tests.sh
```

The MUSCLE3 workflow tests can also be run through pytest. Set
`GCC_MODULES` or `INTEL_MODULES` first, then run:
```bash
pytest tests/muscle3/test_ci_workflows.py -v
```

#### IMAS Integration Tests
Run legacy IMAS integration tests (slow, requires IMAS):
```bash
./tests/run-tests.sh
```

### Direct Pytest Usage

You can also use pytest directly for more control:

```bash
# Run all unit tests
pytest tests/unit/ -v

# Run all integration tests
pytest tests/integration/ -v

# Run only MUSCLE3 tests
pytest tests/ -v -m muscle3

# Run tests excluding MUSCLE3
pytest tests/ -v -m "not muscle3"

# Run specific test file
pytest tests/unit/generators/test_muscle3_python.py -v

# Run with coverage
pytest tests/ --cov=iwrap --cov-report=html

# Run tests in parallel (requires pytest-xdist)
pytest tests/ -n auto
```

## Test Markers

Tests are organized using pytest markers:

- `@pytest.mark.unit` - Fast, isolated unit tests
- `@pytest.mark.integration` - Cross-component integration tests
- `@pytest.mark.muscle3` - Tests requiring MUSCLE3 library
- `@pytest.mark.imas` - Tests requiring IMAS
- `@pytest.mark.slow` - Slow-running tests
- `@pytest.mark.core` - Core functionality (no optional deps)
- `@pytest.mark.generators` - Generator-specific tests

### Running Tests by Marker

```bash
# Run only unit tests
pytest -m unit

# Run only integration tests
pytest -m integration

# Run only MUSCLE3 tests
pytest -m muscle3

# Run core tests (no optional dependencies)
pytest -m core

# Combine markers
pytest -m "unit and not muscle3"
pytest -m "integration or muscle3"
```

## MUSCLE3 Tests

MUSCLE3 tests are **optional** and will be automatically skipped if MUSCLE3 is not installed.

### Installing MUSCLE3

```bash
# Install iWrap with MUSCLE3 support
pip install -e .[muscle3]

# Or install MUSCLE3 separately
pip install muscle3>=0.7.0
```

### MUSCLE3 Test Categories

1. **Unit Tests** (`tests/unit/generators/`)
   - Generator instantiation
   - API version verification
   - Property validation
   - Discovery mechanism

2. **Integration Tests** (`tests/integration/`)
   - Full workflow testing
   - Generator selection
   - Cross-generator compatibility

3. **MUSCLE3-Specific Tests** (`tests/muscle3/`)
   - Actor generation and compilation
   - Wrapped code integration
   - Macro execution
   - Workflow validation
   - Code parameter handling

## Test Fixtures

Common fixtures are provided in `conftest.py`:

### Environment Fixtures
- `muscle3_available` - Check if MUSCLE3 is installed
- `skip_if_no_muscle3` - Skip test if MUSCLE3 unavailable
- `clean_environment` - Clean environment variables
- `mock_imas_environment` - Mock IMAS environment

### Directory Fixtures
- `temp_dir` - Temporary directory for tests
- `temp_project_dir` - Temporary project structure
- `temp_actor_dir` - Temporary actor generation directory

### Settings Fixtures
- `mock_settings` - Mock iWrap settings
- `mock_muscle3_settings` - Mock MUSCLE3 settings

### Generator Fixtures
- `generator_registry` - All discovered generators
- `core_generators` - Only core generators
- `muscle3_generators` - Only MUSCLE3 generators

### Data Fixtures
- `sample_actor_yaml` - Sample actor configuration
- `sample_muscle3_yaml` - Sample MUSCLE3 actor configuration
- `sample_python_file` - Sample Python file
- `sample_yaml_file` - Sample YAML file

## Continuous Integration

### GitHub Actions / GitLab CI

Example CI configuration:

```yaml
test:
  matrix:
    include:
      - name: "Core Tests"
        env:
          MUSCLE3: "false"
      - name: "Core + MUSCLE3 Tests"
        env:
          MUSCLE3: "true"
  
  script:
    - pip install -e .
    - if [ "$MUSCLE3" = "true" ]; then pip install -e .[muscle3]; fi
    - ./tests/run-all-tests-master.sh
```

### Test Reports

Test logs are saved to `/tmp/`:
- `/tmp/iwrap_core_tests.log` - Core unit test output
- `/tmp/iwrap_generator_tests.log` - Generator test output
- `/tmp/iwrap_muscle3_tests.log` - MUSCLE3 test output
- `/tmp/iwrap_integration_tests.log` - Integration test output

### JUnit XML Reports

Generate JUnit XML reports for CI integration:

```bash
pytest tests/ --junitxml=test-results.xml
```

## Writing New Tests

### Unit Test Template

```python
import pytest
from iwrap.generators.actor_generators.your_generator import YourGenerator

@pytest.mark.unit
@pytest.mark.generators
def test_generator_instantiation():
    """Test that generator can be instantiated."""
    generator = YourGenerator()
    assert generator is not None

@pytest.mark.unit
@pytest.mark.muscle3  # If requires MUSCLE3
def test_muscle3_feature(skip_if_no_muscle3):
    """Test MUSCLE3-specific feature."""
    # Test code here
    pass
```

### Integration Test Template

```python
import pytest
from iwrap.generation_engine import Engine

@pytest.mark.integration
def test_full_workflow(temp_dir, mock_settings):
    """Test complete actor generation workflow."""
    engine = Engine(mock_settings)
    # Test workflow here
    pass
```

### Using Fixtures

```python
@pytest.mark.unit
def test_with_fixtures(temp_dir, sample_yaml_file, mock_settings):
    """Test using multiple fixtures."""
    # Fixtures are automatically provided
    assert temp_dir.exists()
    assert sample_yaml_file.exists()
    assert mock_settings.work_dir == str(temp_dir)
```

## Test Coverage

Generate coverage report:

```bash
# HTML report
pytest tests/ --cov=iwrap --cov-report=html
open htmlcov/index.html

# Terminal report
pytest tests/ --cov=iwrap --cov-report=term-missing

# XML report (for CI)
pytest tests/ --cov=iwrap --cov-report=xml
```

## Debugging Tests

### Run Single Test
```bash
pytest tests/unit/generators/test_muscle3_python.py::test_generator_import -v
```

### Drop into PDB on Failure
```bash
pytest tests/ --pdb
```

### Print Output
```bash
pytest tests/ -s  # Show print statements
```

### Verbose Output
```bash
pytest tests/ -vv  # Very verbose
```

## Performance

### Test Execution Time

- **Unit Tests**: < 5 seconds
- **Integration Tests**: < 30 seconds
- **MUSCLE3 Tests**: < 1 minute
- **IMAS Tests**: 5-30 minutes (depending on configuration)

### Parallel Execution

Install pytest-xdist for parallel test execution:

```bash
pip install pytest-xdist

# Run tests in parallel
pytest tests/ -n auto  # Use all CPU cores
pytest tests/ -n 4     # Use 4 workers
```

## Troubleshooting

### MUSCLE3 Tests Always Skipped

Ensure MUSCLE3 is installed:
```bash
python3 -c "import muscle3; print(muscle3.__version__)"
```

Install if missing:
```bash
pip install iwrap[muscle3]
```

### Import Errors

Ensure PYTHONPATH is set:
```bash
export PYTHONPATH=$(pwd):$PYTHONPATH
pytest tests/
```

### Tests Pass Locally but Fail in CI

Check:
1. Python version compatibility
2. Dependency versions
3. Environment variables
4. File permissions
5. PYTHONPATH configuration

## Contributing

When adding new features:

1. **Write tests first** (TDD approach)
2. **Add appropriate markers** (`@pytest.mark.unit`, etc.)
3. **Use existing fixtures** when possible
4. **Document new fixtures** in conftest.py
5. **Update this README** if adding new test categories
6. **Ensure tests pass** with and without MUSCLE3

## Questions?

For questions about the test suite:
- Check existing test files for examples
- Review `conftest.py` for available fixtures
- Consult the [iWrap Documentation](../docs/)
- Open an issue on GitHub/GitLab
