import os
import subprocess
from pathlib import Path
from typing import List


def resolve_path(in_path: str, root_dir: str = None):
    if not in_path:
        return in_path
    out_path = os.path.expandvars( in_path )
    out_path = os.path.expanduser( out_path )
    if root_dir:
        out_path = str( Path( root_dir, out_path ) )
    return out_path


def resolve_variable(in_text: str):
    # check if None
    if not in_text:
        return in_text

    #check if environment var
    if not in_text.startswith( '$' ):
        return in_text

    # removing starting $
    env_variable = in_text[1:]

    try:
        out_text =  os.environ[env_variable]
    except KeyError as ex:
        raise KeyError( f"System variable '{env_variable}' cannot be resolved!" ) from ex

    return out_text


def make_relative(in_path: str, root_dir: str = None):
    if not in_path:
        return in_path

    out_path = in_path
    try:
        out_path = str( Path( in_path ).relative_to( root_dir ) )
        out_path = './' + out_path
    except ValueError:
        pass

    return out_path


def exec_system_cmd(system_cmd: str, return_output:bool = False, working_directory= None,
                    environment=None, output_stream=None):

    if environment:
        environment = {**os.environ, **environment}
    proc = subprocess.Popen( system_cmd, cwd=working_directory, env=environment,
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

    output_value = ''
    for line in proc.stdout:
        line = line.decode( errors='replace' )
        if return_output:
            output_value += line
        else:
            print( line, end='', file=output_stream )

    output_value = output_value.strip()
    return_code = proc.wait()
    if return_code:
        raise RuntimeError( f'ERROR [{return_code}] while executing command: {system_cmd}\n{output_value}' )

    return output_value

def get_all_ids_names() -> List[str]:
    try:
        from data_dictionary import idsinfo
        return idsinfo.IDSInfo().get_ids_names()
    except ImportError:
        from data_dictionary.idsdef import IDSDef
        ids_def = IDSDef()
        return ids_def.get_ids_names()
