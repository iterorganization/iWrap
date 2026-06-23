"""Pytest wrapper for the MUSCLE3 CI workflow tests."""

from __future__ import annotations

import os
import subprocess
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]

ACTOR_CASES = [
    "tests/muscle3/actors/python/basic_python",
    "tests/muscle3/actors/python/restart_python",
    "tests/muscle3/actors/cpp/basic_cpp",
    "tests/muscle3/actors/cpp/basic_mpi_cpp",
    "tests/muscle3/actors/cpp/restart_cpp",
    "tests/muscle3/actors/cpp/restart_mpi_cpp",
    "tests/muscle3/actors/fortran/basic_mpi",
    "tests/muscle3/actors/fortran/code_lifecycle",
    "tests/muscle3/actors/fortran/code_restart",
    "tests/muscle3/actors/fortran/code_restart_mpi",
]


def _run_ci_script(script: str, env: dict[str, str] | None = None) -> str:
    result = subprocess.run(
        ["bash", script],
        cwd=ROOT,
        env=env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )
    assert result.returncode == 0, result.stdout
    return result.stdout


@pytest.fixture(scope="session", autouse=True)
def build_macro_model() -> None:
    """Build the macro model once for all MUSCLE3 actor workflow tests."""
    if not os.environ.get("GCC_MODULES") and not os.environ.get("INTEL_MODULES"):
        pytest.skip("Set GCC_MODULES or INTEL_MODULES to run MUSCLE3 CI tests")

    _run_ci_script("ci/muscle3/build-macro-model.sh")


@pytest.mark.slow
@pytest.mark.parametrize("actor_case", ACTOR_CASES)
def test_muscle3_ci_actor_case(actor_case: str) -> None:
    env = os.environ.copy()
    env["TEST_DIR"] = actor_case

    _run_ci_script("ci/muscle3/test-runner.sh", env=env)
