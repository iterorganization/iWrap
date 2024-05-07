#!/bin/sh --login

echo "-----------------Installing Packages from docs/requirements.txt------------"
python -m pip install --force-reinstall --requirement ./docs/requirements.txt