"""
Integration tests for MUSCLE3 actor generators
Tests the full workflow of MUSCLE3 actor generation
"""
import pytest
import sys
import tempfile
import shutil
from pathlib import Path

# Check if MUSCLE3 is available
muscle3_available = pytest.importorskip("muscle3", reason="MUSCLE3 not installed")


class TestMuscle3Integration:
    """Integration tests for MUSCLE3 functionality"""
    
    @pytest.fixture(autouse=True)
    def setup(self):
        """Setup test environment"""
        iwrap_root = Path(__file__).parent.parent
        if str(iwrap_root) not in sys.path:
            sys.path.insert(0, str(iwrap_root))
        
        # Create temporary directory for test outputs
        self.temp_dir = tempfile.mkdtemp(prefix='iwrap_muscle3_test_')
        yield
        # Cleanup
        if Path(self.temp_dir).exists():
            shutil.rmtree(self.temp_dir)
    
    @pytest.mark.muscle3
    @pytest.mark.integration
    @pytest.mark.slow
    def test_all_muscle3_generators_registered(self):
        """Test that all MUSCLE3 generators are properly registered"""
        from iwrap.generation_engine.engine import Engine
        
        engine = Engine()
        engine.startup()
        
        generators = {g.type: g for g in engine.registered_generators}
        
        # Check all MUSCLE3 generators are present
        assert 'MUSCLE3-Python' in generators
        assert 'MUSCLE3-CPP' in generators
        assert 'MUSCLE3-Fortran' in generators
        
        # Check they have correct API version
        for gen_type in ['MUSCLE3-Python', 'MUSCLE3-CPP', 'MUSCLE3-Fortran']:
            assert generators[gen_type].COMPLIANT_API == '2.1'
    
    @pytest.mark.muscle3
    @pytest.mark.integration
    def test_muscle3_generator_selection(self):
        """Test selecting different MUSCLE3 generators"""
        from iwrap.generation_engine.engine import Engine
        
        engine = Engine()
        engine.startup()
        
        # Test getting each generator type
        for gen_type in ['MUSCLE3-Python', 'MUSCLE3-CPP', 'MUSCLE3-Fortran']:
            generator = Engine.get_generator(gen_type)
            assert generator is not None
            assert generator.type == gen_type
    
    @pytest.mark.muscle3
    @pytest.mark.integration
    def test_core_without_muscle3_still_works(self):
        """Test that core iWrap works even if MUSCLE3 generators fail"""
        from iwrap.generation_engine.engine import Engine
        
        engine = Engine()
        engine.startup()
        
        generators = engine.registered_generators
        
        # At least the core python generator should be available
        generator_types = [g.type for g in generators]
        assert 'python' in generator_types
        
        # And ideally MUSCLE3 ones too
        assert len(generator_types) >= 1


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
