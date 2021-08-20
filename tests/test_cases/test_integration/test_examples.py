from pathlib import Path
import pytest
import subprocess
import os
import logging


LOGGER = logging.getLogger(__name__)

TEST_DIR = ["cp2ds", "cp2ds_cpp", "level2", "level2_cpp"]
if "TEST_DIRS" in os.environ:
    # Redefine the TEST_DIR to include the environment defined names
    TEST_DIR = os.environ.get("TEST_DIRS").split(sep=':')


@pytest.fixture(scope="function", autouse=True, params=TEST_DIR)
def example_dir(request):
    """Prepare the test directory for the current example."""
    test_dir = request.param
    example_dir_path = Path().cwd().parent.resolve() / "examples" / test_dir

    yield example_dir_path

    del example_dir_path


@pytest.mark.slow
@pytest.mark.parametrize('cmd', ["native", "actor", "wf-run", "wf-run STANDALONE"])
def test_make(cmd, example_dir):
    """Executes a \'make\' subprocess and checks whether the return code equals 0."""
    # Logging info: ===== {EXAMPLE DIR} MAKE {CMD} =====
    LOGGER.info(f"{'='.join(['=' for _ in range(25)])} "
                f"{Path(example_dir).name.upper()} "
                f"MAKE {cmd.upper()} "
                f"{'='.join(['=' for _ in range(25)])}")

    make_command = ["make", cmd]
    if cmd == "wf-run STANDALONE":
        os.environ['ACTOR_RUN_MODE'] = 'STANDALONE'
        make_command = ["make", "wf-run"]

    process = subprocess.run(make_command,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE,
                             universal_newlines=True,
                             cwd=example_dir)
    if cmd == "wf-run STANDALONE":
        os.environ['ACTOR_RUN_MODE'] = ''

    LOGGER.info(process.stdout)
    LOGGER.error(process.stderr) if process.stderr != "" else None

    assert process.returncode == 0
