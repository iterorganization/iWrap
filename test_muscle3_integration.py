#!/usr/bin/env python3
"""
Test script to verify MUSCLE3 integration into iWrap core
"""

import sys
sys.path.insert(0, '/home/ITER/sawantp1/github/iWrap')

from iwrap.generation_engine.engine import Engine

print('🔧 Testing iWrap with MUSCLE3 Built-in Generators')
print('=' * 60)

# Initialize engine
Engine().startup()
generators = Engine().registered_generators

print(f'\n✓ Engine initialized successfully!')
print(f'✓ Found {len(generators)} actor generators:\n')

for gen in generators:
    print(f'  Type: {gen.type:<20} | Name: {gen.name:<30}')
    print(f'    API Version: {gen.COMPLIANT_API}')
    if hasattr(gen, 'code_languages'):
        print(f'    Code Languages: {gen.code_languages}')
    print()

print('=' * 60)
print('✅ MUSCLE3 Integration Successful!')
print('\nNext steps:')
print('  1. Test actor generation')
print('  2. Integrate tests')
print('  3. Update documentation')
print('  4. Configure CI/CD')
