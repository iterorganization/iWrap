#!/bin/sh
# Clean IMASPy environment
set -e

# Clean MDSplus model cache
chmod u+w -R ~/.cache/imaspy || true
rm -Rf ~/.cache/imaspy