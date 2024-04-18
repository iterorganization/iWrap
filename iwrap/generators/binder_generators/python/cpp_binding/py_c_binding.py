import logging
import sys
import tempfile
from pathlib import Path
from typing import Set

import jinja2

from iwrap import generators
from iwrap.generation_engine.utils.jinja2_template_processing import process_template_dir
from iwrap.generators.binder_generators import BinderGenerator
from iwrap.settings.platform.platform_settings import PlatformSettings
from iwrap.settings.project import ProjectSettings


class CppBinderGenerator( BinderGenerator ):
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
        return 'Python2CPP'

    @property
    def description(self) -> str:
        return 'Python <--> CPP binding'

    @property
    def actor_language(self) -> str:
        return 'python'

    @property
    def code_languages(self) -> Set[str]:
        return ['cpp', 'fortran']

    @property
    def actor_data_types(self) -> Set[str]:
        return ['legacy']

    @property
    def code_data_types(self) -> Set[str]:
        return ['legacy']

    def __init__(self):
        self.__info_output_stream = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.install_dir: str = None

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    def initialize(self, project_settings: dict):
        install_dir = None

        if project_settings:
            install_dir = project_settings['actor_description'].get( 'install_dir' )

        if not install_dir:
            install_dir = PlatformSettings().directories.actor_install_dir
        self.install_dir: str = str(
            Path( install_dir, ProjectSettings.get_settings().actor_description.actor_name, 'binding' ) )

    def generate(self, project_settings: dict):
        self.temp_dir = tempfile.TemporaryDirectory().name

        def filter_func(x: str) -> bool:
            if "__pycache__" in x:
                return False
            return  True

        #if os.path.isdir(self.install_dir):
        #    shutil.rmtree(self.install_dir)
        process_template_dir('iwrap.generators.binder_generators.python.cpp_binding', 'resources', self.install_dir, project_settings, filter_func=filter_func, output_stream= self.__info_output_stream, )
        process_template_dir( 'iwrap.generators.binder_generators.python', 'common', self.install_dir,
                              project_settings, filter_func=filter_func, output_stream=self.__info_output_stream, )


    def build(self, project_settings: dict):
        ...

    def install(self, project_settings: dict):
        ...

    def cleanup(self, project_settings: dict):
        ...
