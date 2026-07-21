"""
Pytest configuration and shared fixtures for iWrap test suite.

This module provides common fixtures and configuration for all iWrap tests,
including support for conditional MUSCLE3 testing.
"""

import os
import sys
import tempfile
import shutil
from pathlib import Path
import pytest

# Add the project root to the Python path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT))


#######################################################################################################################
# MUSCLE3 Availability Check
#######################################################################################################################

def pytest_configure(config):
    """Configure pytest with custom markers."""
    config.addinivalue_line("markers", "muscle3: marks tests as requiring MUSCLE3 (skipped if not installed)")
    config.addinivalue_line("markers", "unit: marks tests as unit tests")
    config.addinivalue_line("markers", "integration: marks tests as integration tests")
    config.addinivalue_line("markers", "imas: marks tests as requiring IMAS")
    config.addinivalue_line("markers", "slow: marks tests as slow running")
    config.addinivalue_line("markers", "core: marks tests as core functionality (no optional deps)")
    config.addinivalue_line("markers", "generators: marks tests for generator modules")


@pytest.fixture(scope="session")
def muscle3_available():
    """Check if MUSCLE3 is available."""
    try:
        import muscle3
        return True
    except ImportError:
        return False


@pytest.fixture(scope="session")
def skip_if_no_muscle3(muscle3_available):
    """Skip test if MUSCLE3 is not available."""
    if not muscle3_available:
        pytest.skip("MUSCLE3 is not installed")


#######################################################################################################################
# Temporary Directory Fixtures
#######################################################################################################################

@pytest.fixture
def temp_dir():
    """Create a temporary directory for test files."""
    temp_path = tempfile.mkdtemp()
    yield Path(temp_path)
    shutil.rmtree(temp_path, ignore_errors=True)


@pytest.fixture
def temp_project_dir(temp_dir):
    """Create a temporary project directory with standard structure."""
    project_dir = temp_dir / "test_project"
    project_dir.mkdir(parents=True, exist_ok=True)
    
    # Create standard subdirectories
    (project_dir / "src").mkdir()
    (project_dir / "tests").mkdir()
    (project_dir / "docs").mkdir()
    
    yield project_dir
    
    # Cleanup happens automatically via temp_dir cleanup


@pytest.fixture
def temp_actor_dir(temp_dir):
    """Create a temporary directory for actor generation."""
    actor_dir = temp_dir / "actors"
    actor_dir.mkdir(parents=True, exist_ok=True)
    yield actor_dir


#######################################################################################################################
# Mock Settings Fixtures
#######################################################################################################################

@pytest.fixture
def mock_settings(temp_dir):
    """Create mock settings for testing."""
    from iwrap.settings.settings import Settings
    
    settings = Settings()
    settings.work_dir = str(temp_dir)
    settings.actor_name = "test_actor"
    settings.actor_language = "python"
    settings.actor_type = "python"
    
    return settings


@pytest.fixture
def mock_muscle3_settings(temp_dir, muscle3_available):
    """Create mock MUSCLE3 settings for testing."""
    if not muscle3_available:
        pytest.skip("MUSCLE3 is not installed")
    
    from iwrap.settings.settings import Settings
    
    settings = Settings()
    settings.work_dir = str(temp_dir)
    settings.actor_name = "test_muscle3_actor"
    settings.actor_language = "python"
    settings.actor_type = "MUSCLE3-Python"
    
    return settings


#######################################################################################################################
# Sample Data Fixtures
#######################################################################################################################

@pytest.fixture
def sample_actor_yaml():
    """Provide sample actor YAML configuration."""
    return """
actor:
  name: test_actor
  description: Test actor for unit testing
  language: python
  type: python
  
code_parameters:
  - name: test_param
    type: float
    default: 1.0
    description: Test parameter
    
ports:
  inputs:
    - name: input_port
      description: Test input port
  outputs:
    - name: output_port
      description: Test output port
"""


@pytest.fixture
def sample_muscle3_yaml():
    """Provide sample MUSCLE3 actor YAML configuration."""
    return """
actor:
  name: test_muscle3_actor
  description: Test MUSCLE3 actor
  language: python
  type: MUSCLE3-Python
  
code_parameters:
  - name: test_param
    type: float
    default: 1.0
    description: Test parameter
    
ports:
  inputs:
    - name: input_port
      description: Test input port
      operator: F_INIT
  outputs:
    - name: output_port
      description: Test output port
      operator: O_F
"""


#######################################################################################################################
# Generator Registry Fixtures
#######################################################################################################################

@pytest.fixture
def generator_registry():
    """Get the generator registry."""
    from iwrap.generators.actor_generators.base_actor_generator import ActorGenerator
    
    # Force discovery of all generators
    from iwrap.generation_engine import _engine
    
    # Return all discovered generators
    return {gen.get_type(): gen for gen in ActorGenerator.__subclasses__()}


@pytest.fixture
def core_generators(generator_registry):
    """Get only core (non-MUSCLE3) generators."""
    return {
        name: gen for name, gen in generator_registry.items()
        if "MUSCLE3" not in name
    }


@pytest.fixture
def muscle3_generators(generator_registry, muscle3_available):
    """Get only MUSCLE3 generators."""
    if not muscle3_available:
        pytest.skip("MUSCLE3 is not installed")
    
    return {
        name: gen for name, gen in generator_registry.items()
        if "MUSCLE3" in name
    }


#######################################################################################################################
# File System Fixtures
#######################################################################################################################

@pytest.fixture
def sample_python_file(temp_dir):
    """Create a sample Python file for testing."""
    py_file = temp_dir / "test_script.py"
    py_file.write_text("""
def test_function():
    return "Hello, World!"

if __name__ == "__main__":
    print(test_function())
""")
    return py_file


@pytest.fixture
def sample_yaml_file(temp_dir, sample_actor_yaml):
    """Create a sample YAML file for testing."""
    yaml_file = temp_dir / "test_actor.yaml"
    yaml_file.write_text(sample_actor_yaml)
    return yaml_file


@pytest.fixture
def sample_muscle3_yaml_file(temp_dir, sample_muscle3_yaml):
    """Create a sample MUSCLE3 YAML file for testing."""
    yaml_file = temp_dir / "test_muscle3_actor.yaml"
    yaml_file.write_text(sample_muscle3_yaml)
    return yaml_file


#######################################################################################################################
# Environment Fixtures
#######################################################################################################################

@pytest.fixture
def clean_environment(monkeypatch):
    """Provide a clean environment without IMAS/iWrap specific variables."""
    env_vars_to_remove = [
        'IMAS_PREFIX',
        'IMAS_HOME',
        'MUSCLE3_HOME',
        'IWRAP_HOME',
    ]
    
    for var in env_vars_to_remove:
        monkeypatch.delenv(var, raising=False)
    
    yield


@pytest.fixture
def mock_imas_environment(monkeypatch, temp_dir):
    """Mock IMAS environment variables."""
    imas_prefix = temp_dir / "imas"
    imas_prefix.mkdir(parents=True, exist_ok=True)
    
    monkeypatch.setenv('IMAS_PREFIX', str(imas_prefix))
    monkeypatch.setenv('IMAS_HOME', str(imas_prefix))
    
    yield imas_prefix


#######################################################################################################################
# Utility Fixtures
#######################################################################################################################

@pytest.fixture
def caplog_debug(caplog):
    """Set log level to DEBUG for capturing all logs."""
    import logging
    caplog.set_level(logging.DEBUG)
    return caplog


@pytest.fixture
def isolated_filesystem():
    """Create an isolated filesystem for testing."""
    cwd = os.getcwd()
    temp_path = tempfile.mkdtemp()
    os.chdir(temp_path)
    
    yield Path(temp_path)
    
    os.chdir(cwd)
    shutil.rmtree(temp_path, ignore_errors=True)


#######################################################################################################################
# Session Scoped Fixtures
#######################################################################################################################

@pytest.fixture(scope="session")
def project_root():
    """Get the project root directory."""
    return PROJECT_ROOT


@pytest.fixture(scope="session")
def iwrap_version():
    """Get the iWrap version."""
    try:
        from iwrap._version import __version__
        return __version__
    except ImportError:
        return "unknown"


#######################################################################################################################
# Parametrize Helpers
#######################################################################################################################

def pytest_generate_tests(metafunc):
    """Custom test parametrization."""
    # Parametrize actor languages if requested
    if "actor_language" in metafunc.fixturenames:
        languages = ["python"]  # Add more as needed: ["python", "cpp", "fortran"]
        metafunc.parametrize("actor_language", languages)
    
    # Parametrize generator types
    if "generator_type" in metafunc.fixturenames:
        from iwrap.generators.actor_generators.base_actor_generator import ActorGenerator
        
        # Get all generator types
        generator_types = [gen.get_type() for gen in ActorGenerator.__subclasses__()]
        
        # Filter based on markers
        if metafunc.definition.get_closest_marker("muscle3"):
            # Only MUSCLE3 generators
            generator_types = [gt for gt in generator_types if "MUSCLE3" in gt]
        elif metafunc.definition.get_closest_marker("core"):
            # Only core generators
            generator_types = [gt for gt in generator_types if "MUSCLE3" not in gt]
        
        metafunc.parametrize("generator_type", generator_types)


#######################################################################################################################
# Pytest Hooks
#######################################################################################################################

def pytest_collection_modifyitems(config, items):
    """Modify test collection to handle MUSCLE3 markers."""
    try:
        import muscle3
        muscle3_available = True
    except ImportError:
        muscle3_available = False
    
    if not muscle3_available:
        skip_muscle3 = pytest.mark.skip(reason="MUSCLE3 is not installed")
        for item in items:
            if "muscle3" in item.keywords:
                item.add_marker(skip_muscle3)


def pytest_report_header(config):
    """Add custom information to pytest report header."""
    try:
        from iwrap._version import __version__
        version = __version__
    except ImportError:
        version = "unknown"
    
    try:
        import muscle3
        muscle3_version = muscle3.__version__
        muscle3_status = f"installed (v{muscle3_version})"
    except ImportError:
        muscle3_status = "NOT installed"
    
    return [
        f"iWrap version: {version}",
        f"MUSCLE3 status: {muscle3_status}",
        f"Project root: {PROJECT_ROOT}",
    ]
