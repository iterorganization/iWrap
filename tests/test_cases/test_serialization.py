import pytest
from io import StringIO
import yaml
from tests.misc.services import dict_data
from iwrap.settings.serialization import YAMLSerializer


DATA_LIST = ['test_YAMLSerializer_load' + "_Case" + str(case).zfill(2) + '.json' for case in range(3)]
MODULE_PARAMS = ["Case" + str(case).zfill(2) for case, param in enumerate(DATA_LIST)]


@pytest.fixture(scope="module", autouse=True, params=DATA_LIST, ids=MODULE_PARAMS)
def dict_ref(request):
    """Creates a new data model of the reference dictionary."""
    data = dict_data(request.param)
    yield data
    del data


@pytest.fixture(scope="function")
def serializer_object():
    """Creates a new YAMLSerializer object when a test function is called."""
    test_stream = StringIO()
    obj = YAMLSerializer(test_stream)
    yield obj
    del obj


@pytest.mark.usefixtures("dict_ref", "serializer_object")
class TestYAMLSerializer:
    @pytest.mark.parametrize('method', ['stream', 'save', 'load'])
    def test_attribute_present(self, method, serializer_object):
        """Assert if the test attribute is present"""
        assert hasattr(serializer_object, method)

    @pytest.mark.parametrize('node', ['return_type', 'dict_ref'])
    def test_load(self, serializer_object, dict_ref, node):
        """Check that the load method loads the dictionary correctly."""
        serializer_object.stream = StringIO(yaml.dump(dict_ref,
                                                      default_flow_style=False,
                                                      sort_keys=False,
                                                      indent=4,
                                                      explicit_start=True))
        if node == 'dict_ref':
            assert serializer_object.load() == dict_ref
        elif node == 'return_type':
            assert isinstance(serializer_object.load(), dict)

    @pytest.mark.parametrize('node', ['return_type', 'dict_ref'])
    def test_save(self, serializer_object, dict_ref, node):
        """Test if the writing method is saving the dictionary correctly."""
        return_type = serializer_object.save(dict_ref)
        if node == 'dict_ref':
            assert serializer_object.stream.getvalue() == yaml.dump(dict_ref,
                                                                    default_flow_style=False,
                                                                    sort_keys=False,
                                                                    indent=4,
                                                                    explicit_start=True)
        elif node == 'return_type':
            assert return_type is None
