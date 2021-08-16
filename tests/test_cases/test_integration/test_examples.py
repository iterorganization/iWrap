import pytest
import subprocess
import pathlib


TEST_DIR = ["cp2ds", "cp2ds_cpp", "level2", "level2_cpp"]


@pytest.fixture(scope="function", autouse=True, params=TEST_DIR)
def example_dir(request):
    test_dir = request.param
    example_dir_path = pathlib.Path().cwd().parent.resolve() / "examples" / test_dir

    yield example_dir_path

    del example_dir_path


@pytest.mark.parametrize('cmd', ["native", "actor", "wf-run"])
# @pytest.mark.parametrize('cmd', ["native"])
def test_make(cmd, example_dir):
    make_command = ["make", cmd]
    # make_command = ["ls", "-l"]
    process = subprocess.run(make_command,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE,
                             universal_newlines=True,
                             cwd=example_dir)
    print(process.stdout)
    print(process.stderr)
    assert process.returncode == 0

