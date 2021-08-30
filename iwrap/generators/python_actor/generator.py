import os
import shutil
import subprocess
import tempfile
from os.path import join
from pathlib import Path
from typing import Set, List

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generation_engine.utils.jinja2_template_processing import process_template_dir
from iwrap.generators.python_actor.fortran_wrapping import FortranWrapperGenerator
from iwrap.settings.code_description import CodeDescription

import jinja2
import sys

from iwrap.settings.project import ProjectSettings


class PythonActorGenerator(ActorGenerator):

    @property
    def name(self) -> str:
        return 'python'

    @property
    def description(self) -> str:
        return 'Simple Python actor'

    @property
    def actor_data_types(self) -> List[str]:
        return ['legacy']

    @property
    def code_data_types(self) -> List[str]:
        return ['legacy']

    @property
    def code_languages(self) -> Set[str]:
        return {'Fortran', 'cpp'}

    def __init__(self):

        self.__info_output_stream = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.wrapper_generator = FortranWrapperGenerator()
        self.install_dir: str = None
        self.wrapper_dir = None

    def initialize(self):
        install_dir =  ProjectSettings.get_settings().install_dir
        self.install_dir: str = str(Path(install_dir, ProjectSettings.get_settings().actor_description.actor_name))

        self.wrapper_generator = FortranWrapperGenerator()

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    def generate(self):
        self.temp_dir = tempfile.TemporaryDirectory().name
        install_dir = ProjectSettings.get_settings().install_dir
        self.install_dir = str( Path(install_dir, ProjectSettings.get_settings().actor_description.actor_name))

        generation_env = {'temp_dir': self.install_dir}
        actor_settings_dict = ProjectSettings.get_settings().to_dict()
        code_description =  ProjectSettings.get_settings().code_description
        code_description_dict = code_description.to_dict()
        dictionary = {'actor_settings': actor_settings_dict, 'code_description': code_description_dict}

        native_language = code_description.programming_language.lower()

        self.wrapper_dir = native_language + '_wrapper'
        # TO BE CHECKED!!!!


        #if os.path.isdir(self.install_dir):
        #    shutil.rmtree(self.install_dir)

        def filter_func(x: str) -> bool:
            if "_wrapper" in x:
                return native_language + '_wrapper' in x
            return  True

        process_template_dir('iwrap.generators.python_actor', 'resources', self.install_dir, dictionary, filter_func=filter_func, output_stream= self.__info_output_stream,)


        #print('TMP2: ', self.jinja_env.loader.provider.module_path)

        #src = self.jinja_env.loader.provider.module_path + "/" + self.jinja_env.loader.package_path

        # shutil.copytree(src,  self.install_dir, copy_function=self.copy_file)

        #self.wrapper_generator.init(dictionary, generation_env)

        #self.wrapper_generator.generate()
        self.__copy_code_params_files()
        self.__copy_native_lib()
        self.__copy_include()


    def build(self):

        self.cleanup()

        proc = subprocess.Popen( [], executable = "make", cwd=self.install_dir + '/' + self.wrapper_dir,
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

    def __copy_native_lib(self):
        code_description = ProjectSettings.get_settings().code_description
        native_lib_path = code_description.code_path

        root_dir = ProjectSettings.get_settings().root_dir
        native_lib_abs_path = os.path.join( root_dir, native_lib_path )

        destination_dir = os.path.join( self.install_dir, 'lib' )
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        shutil.copy( native_lib_abs_path, destination_dir )

    def __copy_include(self):
        code_description = ProjectSettings.get_settings().code_description
        include_path = code_description.language_specific['include_path']

        root_dir = ProjectSettings.get_settings().root_dir
        include_abs_path = os.path.join( root_dir, include_path )

        destination_dir = os.path.join( self.install_dir, self.wrapper_dir + '/include/' )
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        shutil.copy( include_abs_path, destination_dir )

    def __copy_code_params_files(self):
        code_description = ProjectSettings.get_settings().code_description
        parameters_file = code_description.code_parameters.parameters
        schema_file = code_description.code_parameters.schema

        if not parameters_file:
            return

        if parameters_file and not schema_file:
            raise Exception('Error! Code parameters schema file (XSD) is missing!')

        root_dir = ProjectSettings.get_settings().root_dir
        parameters_file = os.path.join(root_dir, parameters_file)
        schema_file = os.path.join(root_dir,schema_file)
        destination_dir = os.path.join(self.install_dir, 'input')
        if not os.path.isdir( destination_dir ):
            os.makedirs( destination_dir )

        shutil.copy( parameters_file, destination_dir )
        shutil.copy( schema_file, destination_dir )

    def cleanup(self):
        #self.temp_dir.cleanup()
        proc = subprocess.Popen( ['make', 'clean'],
                                 cwd=self.install_dir + '/' + self.wrapper_dir,
                                 encoding='utf-8', text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout.readline():
            print( line, file=self.__info_output_stream, end='' )

        return_code = proc.wait()
        if return_code:
            raise subprocess.CalledProcessError( return_code, 'make clean' )

    def get_code_signature(self) -> str:
        return self.wrapper_generator.get_code_signature()
