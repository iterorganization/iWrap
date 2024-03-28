#!/bin/sh --login

echo "-----------------Installing Packages from docs/requirements.txt------------"
python -m pip install --ignore-installed --requirement ./docs/requirements.txt