from iwrap.settings.project import ProjectSettings
from iwrap.settings.serialization import YAMLSerializer
from tests.misc.services import dict_data
import pytest
import yaml


# Lists of the parameters files and its ids.
DATA_PARAMS_LIST = ['code_description_data' + "_Case" + str(case).zfill(2) + '.json' for case in range(3)]
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
def project_settings(yaml_source):
    """Populates a ProjectSettings object with data."""
    settings = ProjectSettings.get_settings()
    settings.code_description.load(YAMLSerializer(yaml_source))

    yield settings

    del settings


@pytest.mark.usefixtures("dict_ref", "yaml_source", "project_settings")
class TestProjectSettings:
    def test_code_description_structure(self, project_settings, dict_ref):
        """Checks that the CodeDescription dictionary is correctly filled."""
        assert project_settings.code_description.to_dict() == dict_ref
