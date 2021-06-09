from iwrap.settings.project import ProjectSettings
from iwrap.settings.code_description import Argument
import pytest
from misc.services import load_dict


@pytest.fixture
def expected_record(request):
    attribute_name = request.node.funcargs['attribute_name']
    expected_record_dict = load_dict(fspath=request.fspath, fname='code_description_data.json')
    return expected_record_dict.get(attribute_name)


@pytest.fixture
def project_settings():
    settings = ProjectSettings.get_settings()

    settings.name = 'test actor'
    settings.actor_type = 'Python actor'
    settings.data_type = 'LEGACY_IDS'

    yield settings

    del settings


@pytest.mark.parametrize('attribute_name', ['name', 'actor_type', 'data_type'])
def test_base_settings(attribute_name, expected_record, project_settings):
    assert getattr(project_settings, attribute_name) == expected_record
