import logging
import os
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Set, List


from iwrap.generation_engine.utils.jinja2_template_processing import process_template_dir
from iwrap.generators.actor_generators import ActorGenerator
from iwrap.generators.wrapper_generators import WrapperGenerator
from iwrap.settings.project import ProjectSettings

import jinja2
import sys

from iwrap.settings.platform.platform_settings import PlatformSettings



class CppWrapperGenerator(WrapperGenerator):
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
        return 'cpp'

    def __init__(self):

        self.__info_output_stream = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.install_dir: str = None
        self.wrapper_dir = 'wrapper'

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    def initialize(self):
        install_dir =  ProjectSettings.get_settings().actor_description.install_dir
        if not install_dir:
            install_dir = PlatformSettings().default_directories.actor_default_install_dir
        self.install_dir: str = str(Path(install_dir, ProjectSettings.get_settings().actor_description.actor_name, 'wrapper'))

    def generate(self, project_settings: dict):
        self.temp_dir = tempfile.TemporaryDirectory().name
        install_dir = ProjectSettings.get_settings().actor_description.install_dir
        code_description = ProjectSettings.get_settings().code_description
        generation_env = {'temp_dir': self.install_dir}

        native_language = code_description.settings.programming_language.lower()

        # TO BE CHECKED!!!!


        #if os.path.isdir(self.install_dir):
        #    shutil.rmtree(self.install_dir)

        def filter_func(x: str) -> bool:
            if "_wrapper" in x:
                return native_language + '_wrapper' in x
            return  True

        process_template_dir('iwrap.generators.wrapper_generators.cpp_wrapper', 'resources', self.install_dir, project_settings, filter_func=None, output_stream= self.__info_output_stream, )


        #print('TMP2: ', self.jinja_env.loader.provider.module_path)

        #src = self.jinja_env.loader.provider.module_path + "/" + self.jinja_env.loader.package_path

        # shutil.copytree(src,  self.install_dir, copy_function=self.copy_file)



        self.__copy_native_lib(project_settings)
        self.__copy_include(project_settings)


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

        native_lib_path = project_settings['code_description']['code_path']
        shutil.copy( native_lib_path, destination_dir )

    def __copy_include(self, project_settings:dict):

        destination_dir = os.path.join( self.install_dir, 'include' )
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        include_path = project_settings['code_description']['language_specific']['include_path']
        shutil.copy( include_path, destination_dir )


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

    def get_code_signature(self) -> str:
        return self.wrapper_generator.get_code_signature()
