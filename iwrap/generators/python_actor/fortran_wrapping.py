import logging
import sys
import tempfile
from datetime import datetime

import jinja2

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generation_engine.utils.jinja2_template_processing import process_template_dir
from iwrap.settings.project import ProjectSettings


class FortranWrapperGenerator(  ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    TEMPLATE_SUBDIR = 'fortran_wrapper'
    FILES_TO_BE_COPIED = ('src/','','')

    def __init__(self):
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.actor_settings: ProjectSettings = None
        pass

    def init(self, actor_settings: ProjectSettings, generation_env: dict):
        self.actor_settings = actor_settings
        self.temp_dir = generation_env['temp_dir']

    def generate(self):
        actor_settings_dict = self.actor_settings
        process_template_dir(template_pkg='iwrap.generators.python_actor.resources',
                             template_dir='fortran_wrapper',
                             destination_dir=self.temp_dir,
                             dictionary=actor_settings_dict,
                             output_stream=sys.stdout)


    def build(self):
        pass

    def install(self):
        pass

    def cleanup(self):
        pass

    def get_code_signature(self) -> str:
        return 'To be Defined : [ ' + str(datetime.now().time()) + ' ]'
