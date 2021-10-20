import os
from pathlib import Path


def resolve_path(in_path:str, project_root_dir:str=None):
    if not in_path:
        return in_path
    out_path = os.path.expandvars( in_path )
    out_path = os.path.expanduser( out_path )
    if project_root_dir:
        out_path = str(Path( project_root_dir, out_path ))
    return out_path
