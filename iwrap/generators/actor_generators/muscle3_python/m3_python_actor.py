import logging
import os
import shutil
import tempfile
from pathlib import Path
from typing import Set, List


from iwrap.generation_engine.utils.jinja2_template_processing import (
    process_template_dir,
)
from iwrap.generators.actor_generators import ActorGenerator
from iwrap.settings.project import ProjectSettings

import jinja2
import sys

from iwrap.settings.platform.platform_settings import PlatformSettings

from iwrap.generators.actor_generators.muscle3_common import m3_utils


class PythonActorGenerator(ActorGenerator):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    COMPLIANT_API = "2.1"

    @property
    def type(self) -> str:
        return "MUSCLE3-Python"

    @property
    def name(self) -> str:
        return "MUSCLE3 (Python)"

    @property
    def description(self) -> str:
        return "Wrapping Python code into MUSCLE3 micro model"

    @property
    def actor_language(self) -> List[str]:
        return "python"

    @property
    def actor_data_types(self) -> List[str]:
        return ["legacy"]

    @property
    def code_data_types(self) -> List[str]:
        return ["legacy"]

    @property
    def code_languages(self) -> Set[str]:
        return {"python"}

    def __init__(self):

        self.__info_output_stream = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.install_dir: str = None
        self.wrapper_dir = "wrapper"

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    def validate(self, project_settings: dict = None):
        m3_utils.validate(project_settings)

    def initialize(self, project_settings: dict = None):
        install_dir = None

        if project_settings:
            install_dir = project_settings["actor_description"].get("install_dir")

        if not install_dir:
            install_dir = PlatformSettings().directories.actor_install_dir
        self.install_dir: str = str(
            Path(
                install_dir, ProjectSettings.get_settings().actor_description.actor_name
            )
        )

    def generate(self, project_settings: dict):
        self.cleanup(project_settings)
        os.makedirs(self.install_dir, exist_ok=True)
        current_path = os.path.dirname(os.path.realpath(__file__))
        process_template_dir(
            None,
            current_path + "/resources",
            self.install_dir,
            project_settings,
            filter_func=m3_utils.template_filter_func,
            output_stream=self.__info_output_stream,
        )

        m3_utils.copy_native_lib(self.install_dir, project_settings, "wrapped_code")
        m3_utils.copy_code_params_files(self.install_dir, project_settings)
        m3_utils.copy_build_info(self.install_dir, project_settings)

    def build(self, project_settings: dict): ...

    def install(self, project_settings: dict): ...

    def cleanup(self, project_settings: dict):
        shutil.rmtree(self.install_dir, ignore_errors=True)
