from .dict_based_handler import DictBasedHandler
import logging
from io import StringIO
from typing import Set

class FortranNamelistHandler(DictBasedHandler):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    @property
    def formats(self) -> Set[str]:
        return {'namelist'}

    def to_dict(self):
        import f90nml
        namelist_object = f90nml.reads(self._parameters_str)
        return namelist_object.todict()

    def from_dict(self, dict):
        import f90nml

        namelist_object = f90nml.Namelist(dict)
        string_buffer = StringIO()
        namelist_object.write(string_buffer)
        self._parameters_str = string_buffer.getvalue()

    def __init__(self):
        super(FortranNamelistHandler, self).__init__()

    def validate(self):
        import f90nml
        from jsonschema import validate
        import json
        if not self._parameters_str or self._new_path_set:
            self.initialize(self._parameters_path, self._schema_path)

        namelist_dict = f90nml.reads(self._parameters_str).todict()
        schema_dict = json.loads(self._schema_str)
        # jsonschema.validate
        # both json and namelist are dict-based thus namelist can be validated against jsonschema
        validate(namelist_dict, schema_dict)