import py
import pathlib
import json


def dict_tuple(data: dict) -> dict:
    """Iterates through the dictionary and evaluates the possible tuple marked as a string."""
    for key, value in data.copy().items():
        if isinstance(value, dict):  # For sub-dictionary
            data[key] = dict_tuple(value)
        elif value.startswith("(") and value.endswith(")"):  # For possible tuple
            try:
                data[key] = eval(value)
            except NotImplementedError:
                pass
    return data


def load_dict(fspath: py.path.local = None, fname: str = 'tests_data.json') -> dict:
    if fspath is None:
        raise FileNotFoundError("Wrong path to data!")
    fspath = pathlib.Path(fspath).parent / fname
    with open(fspath, 'r') as stream:
        return dict_tuple(json.load(stream))


def tuple2json(data: dict) -> dict:
    """Iterates through the dictionary and casts a tuple as a string."""
    for key, value in data.copy().items():
        if isinstance(value, dict):     # For sub-dictionary
            data[key] = tuple2json(value)
        elif isinstance(value, tuple):  # For tuple
            data[key] = str(value)
    return data


def save_dict(data: dict, fname: str = 'tests_data.json') -> None:
    data_modified = tuple2json(data)
    with open(fname, "w+") as file:
        file.write(json.dumps(data_modified, indent=4))


def dict_data(fname: str = None, file_dir=__file__):
    """Loads data from a JSON file."""
    if fname is None:
        return
    dict_path = pathlib.Path(file_dir).parent.absolute() / fname
    with open(dict_path, "r") as file:
        return parametrize_dict(json.load(file))


def parametrize_dict(data: dict) -> dict:
    """Finds sub-dictionaries and rebuilds data."""
    ret_dir = {}
    if not all(isinstance(value, dict) for value in data.values()):
        ret_dir["Case_0"] = data
    else:
        ret_dir = data
    return ret_dir


