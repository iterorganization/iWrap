import sys
from typing import Set, List

from iwrap.generators.actor_generators import ActorGenerator


class EmptyGenerator( ActorGenerator ):

    @property
    def name(self) -> str:
        return 'empty'

    @property
    def description(self) -> str:
        return 'An empty actor generator'

    @property
    def actor_language(self) -> str:
        return 'actor_language_1'

    @property
    def actor_data_types(self) -> List[str]:
        return ['actor_data_type_1', 'actor_data_type_2']

    @property
    def code_data_types(self) -> List[str]:
        return ['code_data_type_1', 'code_data_type_2']

    @property
    def code_languages(self) -> Set[str]:
        return {'CPP', 'Python'}

    def __init__(self):
        ...

    def initialize(self):
        ...

    def configure(self, info_output_stream=sys.stdout):
        ...

    def generate(self):
        ...

    def build(self):
        ...

    def install(self):
        ...

    def cleanup(self):
        ...

    def get_code_signature(self) -> str:
        return 'TO BE DEFINED'
