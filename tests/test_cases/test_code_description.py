from iwrap.settings.project import ProjectSettings
from iwrap.settings.serialization import YAMLSerializer
from tests.misc.services import dict_data
from typing import Union
from io import StringIO
import pytest
import yaml


# Lists of the parameters files and its ids.
DATA_PARAMS_LIST = ['code_description_data' + "_Case" + str(case).zfill(2) + '.json' for case in range(1)]
DATA_PARAMS_IDS = ["Case" + str(case).zfill(2) for case, param in enumerate(DATA_PARAMS_LIST)]


@pytest.fixture(scope="module", autouse=True, params=DATA_PARAMS_LIST, ids=DATA_PARAMS_IDS)
def dict_ref(request):
    """Creates a new data model of the reference dictionary."""
    data = dict_data(request.param)
    yield data
    del data


@pytest.fixture(scope="class")
def yaml_source(dict_ref):
    """Creates a data source in the form of YAML"""
    return yaml.dump(dict_ref, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True)


@pytest.fixture(scope="function")
def code_description(yaml_source):
    """Populates a ProjectSettings object with data."""
    # Setup:
    settings = ProjectSettings.get_settings()
    yield settings.code_description
    # Teardown:
    ProjectSettings._settings = None
    del settings


@pytest.mark.usefixtures("dict_ref", "yaml_source", "code_description")
class TestCodeDescription:
    def test_to_dict(self, code_description, yaml_source, dict_ref):
        """Checks that the CodeDescription dictionary is correctly filled."""
        code_description.load(YAMLSerializer(yaml_source))
        assert code_description.to_dict() == dict_ref

    def keys_count(self, dict_to_check, dict_reference: dict):
        assert len(dict_reference) == len(vars(dict_to_check))

    def keys_identity(self, dict_to_check, dict_reference: dict):
        for key in vars(dict_to_check):
            assert key in dict_reference

    def nested_values(self, nested: Union[list, dict], reference: Union[list, dict]):
        """Visit all nested nodes and check them against the reference."""
        if isinstance(nested, list):
            for pos, item in enumerate(nested):
                self.nested_values(item, reference[pos])
        if isinstance(nested, dict):
            for key, value in nested.items():
                if hasattr(value, "__dict__"):
                    self.nested_values(vars(value), reference[key])
                elif isinstance(value, list):
                    self.nested_values(value, reference[key])
                elif isinstance(value, dict):
                    self.nested_values(value, reference[key])
                else:
                    assert value == reference[key]
        else:
            assert nested == reference

    @pytest.mark.parametrize('function', ["keys_count", "keys_identity", "nested_values"])
    def test_from_dict(self, function, code_description, dict_ref):
        code_description.from_dict(dict_ref)
        if function == "nested_values":
            getattr(TestCodeDescription, function)(self, vars(code_description), dict_ref)
        else:
            getattr(TestCodeDescription, function)(self, code_description, dict_ref)

    def test_save(self, code_description, dict_ref):
        pass

    def test_clear(self, code_description, dict_ref):
        pass
