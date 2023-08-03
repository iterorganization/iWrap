#!/bin/sh --login

echo "-----------Create Python Virtual ENV-------------"
# Remove Virtual env if already exists
rm -rf venv
python -m venv --system-site-packages venv
