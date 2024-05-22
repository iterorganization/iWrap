import logging
import os
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Set, List

from iwrap import generators
from iwrap.generation_engine.utils.jinja2_template_processing import process_template_dir
from iwrap.generators.actor_generators import ActorGenerator
from iwrap.settings.project import ProjectSettings

import jinja2
import sys

from iwrap.settings.platform.platform_settings import PlatformSettings


class PythonActorGenerator(ActorGenerator):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    COMPLIANT_API = generators.API_VERSION
    """ The API version compatible with this plugin. 
    It is a built-in plugin, so it should be always up to date
    """

    @property
    def type(self) -> str:
        return 'python'

    @property
    def name(self) -> str:
        return 'python'

    @property
    def description(self) -> str:
        return 'python'

    @property
    def actor_language(self) -> List[str]:
        return 'python'

    @property
    def actor_data_types(self) -> List[str]:
        return ['legacy']

    @property
    def code_data_types(self) -> List[str]:
        return ['legacy']

    @property
    def code_languages(self) -> Set[str]:
        return {'fortran', 'cpp', 'java'}

    def __init__(self):

        self.__info_output_stream = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.install_dir: str = None

    def initialize(self, project_settings: dict = None):
        install_dir = None

        if project_settings:
            install_dir = project_settings['actor_description'].get( 'install_dir' )

        if not install_dir:
            install_dir = PlatformSettings().directories.actor_install_dir
        self.install_dir: str = str(Path(install_dir, ProjectSettings.get_settings().actor_description.actor_name))

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    def generate(self, project_settings: dict):
        self.cleanup()
        os.makedirs( self.install_dir, exist_ok=True )

        self.temp_dir = tempfile.TemporaryDirectory().name
        generation_env = {'temp_dir': self.install_dir}

        def filter_func(x: str) -> bool:
            if "__pycache__" in x:
                return False

            return  True

        current_path = os.path.dirname( os.path.realpath( __file__ ) )
        process_template_dir( None, current_path + '/resources', self.install_dir, project_settings,
                              filter_func=filter_func,
                              output_stream=self.__info_output_stream, )

        self.__copy_code_params_files(project_settings)

    def build(self, project_settings: dict):
        ...

    def install(self, project_settings: dict):
        pass
        # cleanup leftovers (if any)
        # if os.path.isdir( self.install_dir ):
        #    shutil.rmtree( self.install_dir )


    def __copy_code_params_files(self, project_settings:dict):
        code_parameters = project_settings['code_description']['implementation']['code_parameters']
        if not code_parameters:
            return

        parameters_file = code_parameters.get('parameters')
        schema_file = code_parameters.get('schema')

        if not parameters_file:
            return

        if parameters_file and not schema_file:
            raise Exception('Error! Code parameters schema file (XSD) is missing!')

        destination_dir = os.path.join(self.install_dir, 'input')
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        shutil.copy( parameters_file, destination_dir )
        shutil.copy( schema_file, destination_dir )

    def cleanup(self):
        if os.path.isdir(self.install_dir):
            shutil.rmtree(self.install_dir)
