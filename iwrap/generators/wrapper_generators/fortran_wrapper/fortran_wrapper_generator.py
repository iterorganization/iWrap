import logging
import os
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Set, List


from iwrap.generation_engine.utils.jinja2_template_processing import process_template_dir
from iwrap.generators.wrapper_generators import WrapperGenerator
from iwrap.settings.project import ProjectSettings

import jinja2
import sys

from iwrap.settings.platform.platform_settings import PlatformSettings



class FortranWrapperGenerator(WrapperGenerator):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


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
        return 'fortran'

    def __init__(self):

        self.__info_output_stream = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.install_dir: str = None
        self.wrapper_dir = 'wrapper'

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    def initialize(self):
        install_dir =  ProjectSettings.get_settings().actor_description._install_dir
        if not install_dir:
            install_dir = PlatformSettings().directories.actor_install_dir
        self.install_dir: str = str(Path(install_dir, ProjectSettings.get_settings().actor_description.actor_name, self.wrapper_dir))



    def generate(self, project_settings: dict):
        self.temp_dir = tempfile.TemporaryDirectory().name

        code_description = ProjectSettings.get_settings().code_description
        generation_env = {'temp_dir': self.install_dir}

        native_language = code_description.implementation.programming_language.lower()

        def filter_func(x: str) -> bool:
            if "_wrapper" in x:
                return native_language + '_wrapper' in x
            return  True

        process_template_dir('iwrap.generators.wrapper_generators.fortran_wrapper', 'resources', self.install_dir, project_settings, filter_func=None, output_stream= self.__info_output_stream, )

        self.__copy_native_lib(project_settings)
        self.__copy_include(project_settings)
        self.__copy_extra_libs(project_settings)

    def build(self):

        self.cleanup()

        proc = subprocess.Popen( [], executable = "make", cwd=self.install_dir,
                                 encoding='utf-8', text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout:
            print( line, file=self.__info_output_stream, end='' )

        return_code = proc.wait()
        if return_code:
            raise subprocess.CalledProcessError( return_code, 'make' )



    def install(self):
        # cleanup leftovers (if any)
        if os.path.isdir( self.install_dir ):
            shutil.rmtree( self.install_dir )

    def __copy_native_lib(self, project_settings:dict):
        destination_dir = os.path.join( self.install_dir, 'lib' )
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        native_lib_path = project_settings['code_description']['implementation']['code_path']
        shutil.copy( native_lib_path, destination_dir )

    def __copy_include(self, project_settings:dict):

        destination_dir = os.path.join( self.install_dir, 'include' )
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        include_path = project_settings['code_description']['implementation']['include_path']
        shutil.copy( include_path, destination_dir )

    def __copy_extra_libs(self, project_settings: dict):

        settings = project_settings['code_description']['settings']
        extra_libraries = settings.get( 'extra_libraries' )
        if not extra_libraries:
            return

        libraries = extra_libraries.get( 'path_defined' )
        if not libraries:
            return

        destination_dir = os.path.join( self.install_dir, 'extra-libs' )
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        for library_path in libraries:
            shutil.copy( library_path, destination_dir )

    def cleanup(self):
        #self.temp_dir.cleanup()
        proc = subprocess.Popen( ['make', 'clean'],
                                 cwd=self.install_dir,
                                 encoding='utf-8', text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout.readline():
            print( line, file=self.__info_output_stream, end='' )

        return_code = proc.wait()
        if return_code:
            raise subprocess.CalledProcessError( return_code, 'make clean' )
