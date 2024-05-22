from .dict_based_handler import DictBasedHandler
import logging
import json
from jsonschema import validate
from typing import Set

class JsonHandler(DictBasedHandler):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    @property
    def formats(self) -> Set[str]:
        return {'json'}

    def __init__(self):
        super(JsonHandler, self).__init__()

    def from_dict(self, dict):
        self._parameters_str = json.dumps(dict, indent=4)

    def to_dict(self):
        return json.loads(self._parameters_str)

    def validate(self):
        if not self._parameters_str or self._new_path_set:
            self.initialize(self._parameters_path, self._schema_path)

        # jsonschema.validate
        validate(json.loads(self._parameters_str), json.loads(self._schema_str))