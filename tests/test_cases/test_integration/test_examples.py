import pytest
import sys
import subprocess
import pathlib
import os


TEST_DIR = ["cp2ds", "cp2ds_cpp", "level2", "level2_cpp"]


@pytest.fixture(scope="function", autouse=True, params=TEST_DIR)
def example_dir(request):
    test_dir = request.param
    example_dir_path = pathlib.Path().cwd().parent.resolve() / "examples" / test_dir

    yield example_dir_path

    del example_dir_path


@pytest.mark.slow
@pytest.mark.parametrize('cmd', ["native", "actor", "wf-run", "wf-run STANDALONE"])
def test_make(cmd, example_dir):
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
    print(process.stdout, file=sys.stdout)
    print(process.stderr, file=sys.stderr)
    assert process.returncode == 0

