"""
Unit tests for MUSCLE3 C++ actor generator
"""
import pytest
import sys
from pathlib import Path

# Check if MUSCLE3 is available
muscle3_available = pytest.importorskip("muscle3", reason="MUSCLE3 not installed")


class TestMuscle3CppGenerator:
    """Test suite for MUSCLE3 C++ actor generator"""
    
    @pytest.fixture(autouse=True)
    def setup(self):
        """Setup test environment"""
        iwrap_root = Path(__file__).parent.parent.parent
        if str(iwrap_root) not in sys.path:
            sys.path.insert(0, str(iwrap_root))
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_import(self):
        """Test that MUSCLE3 C++ generator can be imported"""
        from iwrap.generators.actor_generators.muscle3_cpp.m3_cpp_actor import CppActorGenerator
        assert CppActorGenerator is not None
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_instantiation(self):
        """Test MUSCLE3 C++ generator instantiation"""
        from iwrap.generators.actor_generators.muscle3_cpp.m3_cpp_actor import CppActorGenerator
        generator = CppActorGenerator()
        assert generator is not None
        assert generator.type == 'MUSCLE3-Cpp'
        assert generator.name == 'MUSCLE3 (C++)'
        assert generator.actor_language == 'python'
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_api_version(self):
        """Test that generator has correct API version"""
        from iwrap.generators.actor_generators.muscle3_cpp.m3_cpp_actor import CppActorGenerator
        generator = CppActorGenerator()
        assert generator.COMPLIANT_API == '2.1'
    
    @pytest.mark.muscle3
    @pytest.mark.unit
    def test_generator_properties(self):
        """Test generator properties"""
        from iwrap.generators.actor_generators.muscle3_cpp.m3_cpp_actor import CppActorGenerator
        generator = CppActorGenerator()
        
        assert 'cpp' in generator.code_languages
        assert 'legacy' in generator.actor_data_types
        assert 'legacy' in generator.code_data_types


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
