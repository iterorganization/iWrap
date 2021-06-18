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
class TestProjectSettings:
    def test_to_dict(self, code_description, yaml_source, dict_ref):
        """Checks that the CodeDescription dictionary is correctly filled."""
        code_description.load(YAMLSerializer(yaml_source))
        assert code_description.to_dict() == dict_ref

    @pytest.mark.parametrize('param', [1, 2, 3], ids=["attr_count", "attr_compatibility", "val_compatibility"])
    def test_from_dict(self, param, code_description, dict_ref):
        code_description.from_dict(dict_ref)
        if param == 1:
            assert len(dict_ref) == len(vars(code_description))
        else:
            for key in vars(code_description):
                if param == 2:
                    assert key in dict_ref
                else:
                    value = getattr(code_description, key)
                    if isinstance(value, Union[str, list, dict, int, bool, None].__args__):
                        assert value == dict_ref[key]
                    else:
                        assert vars(value) == dict_ref[key]

    def test_save(self, code_description, dict_ref):
        pass

    def test_clear(self, code_description, dict_ref):
        pass
