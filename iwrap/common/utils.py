import os
from pathlib import Path


def resolve_path(in_path:str, root_dir:str=None):
    if not in_path:
        return in_path
    out_path = os.path.expandvars( in_path )
    out_path = os.path.expanduser( out_path )
    if root_dir:
        out_path = str(Path( root_dir, out_path ))
    return out_path


def make_relative(in_path:str, root_dir:str=None):
    if not in_path:
        return in_path

    out_path = in_path
    try:
        out_path = str(Path(in_path).relative_to(root_dir))
        out_path = './' + out_path
    except ValueError:
        pass

    return out_path

