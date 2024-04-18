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
from iwrap.generators.wrapper_generators import WrapperGenerator
from iwrap.settings.project import ProjectSettings

import jinja2
import sys

from iwrap.settings.platform.platform_settings import PlatformSettings


class JavaWrapperGenerator(WrapperGenerator):
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
        return 'Simple Python actor'

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
    def code_language(self) -> str:
        return 'java'

    def __init__(self):

        self.__info_output_stream = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.install_dir: str = None
        self.wrapper_dir = 'wrapper'

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    def validate(self, project_settings: dict):
        ...

    def initialize(self, project_settings: dict):
        install_dir = None

        if project_settings:
            install_dir = project_settings['actor_description'].get( 'install_dir' )

        if not install_dir:
            install_dir = PlatformSettings().directories.actor_install_dir
        self.install_dir: str = str(Path(install_dir, ProjectSettings.get_settings().actor_description.actor_name, 'wrapper'))

    def generate(self, project_settings: dict):

        process_template_dir('iwrap.generators.wrapper_generators.java_wrapper', 'resources', self.install_dir, project_settings, filter_func=None, output_stream= self.__info_output_stream, )

        self.__copy_native_lib(project_settings)

    def build(self, project_settings: dict):

        proc = subprocess.Popen( [], executable = "make", cwd=self.install_dir,
                                 encoding='utf-8', text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout:
            print( line, file=self.__info_output_stream, end='' )

        return_code = proc.wait()
        if return_code:
            raise subprocess.CalledProcessError( return_code, 'make' )



    def install(self, project_settings: dict):
        # cleanup leftovers (if any)
        #if os.path.isdir( self.install_dir ):
        #    shutil.rmtree( self.install_dir )
        ...

    def __copy_native_lib(self, project_settings:dict):
        destination_dir = os.path.join( self.install_dir, 'lib' )
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        native_lib_path = project_settings['code_description']['implementation']['code_path']
        shutil.copy( native_lib_path, destination_dir )

    def cleanup(self, project_settings: dict):
        #self.temp_dir.cleanup()
        proc = subprocess.Popen( ['make', 'clean'],
                                 cwd=self.install_dir,
                                 encoding='utf-8', text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout.readline():
            print( line, file=self.__info_output_stream, end='' )

        return_code = proc.wait()
        if return_code:
            raise subprocess.CalledProcessError( return_code, 'make clean' )
