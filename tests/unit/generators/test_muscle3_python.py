"""
Unit tests for MUSCLE3 Python actor generator
"""
import pytest
import sys
from pathlib import Path

# Check if MUSCLE3 is available
muscle3_available = pytest.importorskip("muscle3", reason="MUSCLE3 not installed")


class TestMuscle3PythonGenerator:
    """Test suite for MUSCLE3 Python actor generator"""
    
    @pytest.fixture(autouse=True)
    def setup(self):
        """Setup test environment"""
        # Add iWrap to path if needed
        iwrap_root = Path(__file__).parent.parent.parent
        if str(iwrap_root) not in sys.path:
            sys.path.insert(0, str(iwrap_root))
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_import(self):
        """Test that MUSCLE3 Python generator can be imported"""
        from iwrap.generators.actor_generators.muscle3_python.m3_python_actor import PythonActorGenerator
        assert PythonActorGenerator is not None
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_instantiation(self):
        """Test MUSCLE3 Python generator instantiation"""
        from iwrap.generators.actor_generators.muscle3_python.m3_python_actor import PythonActorGenerator
        generator = PythonActorGenerator()
        assert generator is not None
        assert generator.type == 'MUSCLE3-Python'
        assert generator.name == 'MUSCLE3 (Python)'
        assert generator.actor_language == 'python'
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_api_version(self):
        """Test that generator has correct API version"""
        from iwrap.generators.actor_generators.muscle3_python.m3_python_actor import PythonActorGenerator
        generator = PythonActorGenerator()
        assert generator.COMPLIANT_API == '2.1'
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_properties(self):
        """Test generator properties"""
        from iwrap.generators.actor_generators.muscle3_python.m3_python_actor import PythonActorGenerator
        generator = PythonActorGenerator()
        
        assert 'python' in generator.code_languages
        assert 'legacy' in generator.actor_data_types
        assert 'legacy' in generator.code_data_types
        assert generator.description == 'Wrapping Python code into MUSCLE3 micro model'
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_discovery(self):
        """Test that MUSCLE3 generator is discovered by engine"""
        from iwrap.generation_engine.engine import Engine
        
        engine = Engine()
        engine.startup()
        
        generators = engine.registered_generators
        generator_types = [g.type for g in generators]
        
        assert 'MUSCLE3-Python' in generator_types
        assert 'MUSCLE3-CPP' in generator_types
        assert 'MUSCLE3-Fortran' in generator_types


class TestMuscle3CommonUtils:
    """Test suite for MUSCLE3 common utilities"""
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_m3_utils_import(self):
        """Test that m3_utils can be imported"""
        from iwrap.generators.actor_generators.muscle3_common import m3_utils
        assert m3_utils is not None
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_template_filter_func(self):
        """Test template filter function"""
        from iwrap.generators.actor_generators.muscle3_common.m3_utils import template_filter_func
        
        assert template_filter_func(['normal_file.py']) == True
        assert template_filter_func(['__pycache__/file.pyc']) == False
        assert template_filter_func(['macros/macro.j2']) == False
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_validate_function(self):
        """Test validation function for MUSCLE3 constraints"""
        from iwrap.generators.actor_generators.muscle3_common.m3_utils import validate
        
        # Valid case: no IDS arguments in init/finalize
        valid_settings = {
            'code_description': {
                'implementation': {
                    'subroutines': {
                        'init': {'arguments': []},
                        'finalize': {'arguments': []}
                    }
                }
            }
        }
        
        # Should not raise
        validate(valid_settings)
        
        # Invalid case: IDS arguments in init
        invalid_settings = {
            'code_description': {
                'implementation': {
                    'subroutines': {
                        'init': {'arguments': [{'name': 'equilibrium', 'type': 'input'}]},
                        'finalize': {'arguments': []}
                    }
                }
            }
        }
        
        # Should raise ValueError
        with pytest.raises(ValueError, match='MUSCLE3 actor generator cannot handle INIT/FINALIZE'):
            validate(invalid_settings)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
