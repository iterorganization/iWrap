import pytest
from importlib import import_module
from misc.services import load_dict


@pytest.fixture
def expected_record(request):
    class_name = request.node.funcargs['class_name']
    field = request.node.funcargs['field']
    expected_record_dict = load_dict(fspath=request.fspath)
    return expected_record_dict.get(class_name, dict()).get(field, None)


@pytest.mark.parametrize('class_name', ['File', 'XMLFile', 'XSDFile'])
@pytest.mark.parametrize('field', ['_extension', '_title'])
def test_class_field(class_name, field, expected_record):
    instance = getattr(import_module('iwrap.gui.settings.code_parameters_pane'), class_name)
    record = getattr(instance, field)
    assert record == expected_record


def test_instance():
    instance = getattr(import_module('iwrap.gui.settings.code_parameters_pane'), "File")
    assert instance()
