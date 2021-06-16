import pathlib
import json


def dict_data(fname: str = None, file_dir=pathlib.Path.cwd() / "test_data"):
    """Loads data from a JSON file."""
    if fname is None:
        raise FileNotFoundError("Wrong path to data!")
    dict_path = pathlib.Path(file_dir).absolute() / fname
    with open(dict_path, "r") as file:
        return json.load(file)
