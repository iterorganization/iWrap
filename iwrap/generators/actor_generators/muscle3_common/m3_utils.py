import os
import shutil
from typing import List

import yaml
from .dumper import CustomDumper


def template_filter_func(templates_list: List[str]) -> bool:
    if "__pycache__" in templates_list:
        return False

    if "macros" in templates_list:
        return False

    return True

def copy_native_lib(install_dir: str, project_settings:dict, destination_dir:str):

    native_lib_path = project_settings['code_description'].get('implementation', {}).get('code_path')
    if not native_lib_path:
        return

    destination_dir = os.path.join( install_dir, destination_dir)
    if not os.path.isdir( destination_dir ):
        os.makedirs( destination_dir )

    shutil.copy( native_lib_path, destination_dir )

def copy_include(install_dir: str, project_settings:dict):

    include_path = project_settings['code_description'].get('implementation', {}).get('include_path')
    if not include_path:
        return

    destination_dir = os.path.join( install_dir, 'include' )
    if not os.path.isdir( destination_dir ):
        os.makedirs( destination_dir )

    shutil.copy( include_path, destination_dir )

def copy_extra_libs(install_dir: str, project_settings: dict):

    libraries = project_settings['code_description'].get('settings', {}).get('extra_libraries', {}).get('path_defined')

    if not libraries:
        return

    destination_dir = os.path.join( install_dir, 'extra-libs' )
    if not os.path.isdir( destination_dir ):
        os.makedirs( destination_dir )

    for library_path in libraries:
        shutil.copy( library_path, destination_dir )
        

def copy_code_params_files(install_dir: str, project_settings:dict):
    code_parameters = project_settings['code_description'].get('implementation', {}).get('code_parameters')
    if not code_parameters:
        return

    parameters_file = code_parameters.get('parameters')
    schema_file = code_parameters.get('schema')

    if not parameters_file:
        return

    if parameters_file and not schema_file:
        raise Exception('Error! Code parameters schema file (XSD) is missing!')

    destination_dir = os.path.join(install_dir, 'input')
    if not os.path.isdir( destination_dir ):
        os.makedirs( destination_dir )

    shutil.copy( parameters_file, destination_dir )
    shutil.copy( schema_file, destination_dir )

def copy_build_info(install_dir: str, project_settings:dict):
    output_dict = dict((key, project_settings[key]) for key in project_settings if key in ['actor_description','code_description','build_info'])
    with open(f'{install_dir}/{project_settings["actor_description"]["actor_name"]}.yaml', "w") as file:
        yaml.dump(output_dict, file, Dumper=CustomDumper)


def validate(project_settings: dict = None):

    init_arguments = project_settings['code_description'] \
        .get('implementation', {}) \
        .get('subroutines', {}) \
        .get('init', {})\
        .get('arguments')

    finalize_arguments = project_settings['code_description'] \
        .get('implementation', {}) \
        .get('subroutines', {}) \
        .get('finalize', {})\
        .get('arguments')

    if init_arguments or finalize_arguments:
        raise ValueError( 'MUSCLE3 actor generator cannot handle INIT/FINALIZE methods with IDS arguments' )