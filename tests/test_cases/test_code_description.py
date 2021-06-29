from iwrap.settings.project import ProjectSettings
from iwrap.settings.serialization import YAMLSerializer
from tests.misc.services import dict_data
from typing import Union, Tuple
from io import StringIO
import pytest
import yaml


# Lists of the parameters files and its ids.
DATA_PARAMS_LIST = ['code_description_data' + "_Case" + str(case).zfill(2) + '.json' for case in range(3)]
DATA_PARAMS_IDS = ["Case" + str(case).zfill(2) for case, param in enumerate(DATA_PARAMS_LIST)]


@pytest.fixture(scope="function", autouse=True, params=DATA_PARAMS_LIST, ids=DATA_PARAMS_IDS)
def dict_ref(request):
    """Creates a new data model of the reference dictionary."""
    data = dict_data(request.param)
    yield data
    del data


@pytest.fixture(scope="module", autouse=True)
def dict_clear_ref():
    """Creates a new data model of the reference dictionary."""
    data = dict_data("code_description_data_CaseClear.json")
    yield data
    del data


@pytest.fixture(scope="function")
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


@pytest.mark.usefixtures("dict_ref", "dict_clear_ref", "yaml_source", "code_description")
class TestCodeDescription:
    def test_to_dict(self, code_description, yaml_source, dict_ref):
        """Checks that the CodeDescription dictionary is correctly filled."""
        code_description.load(YAMLSerializer(yaml_source))
        assert code_description.to_dict() == dict_ref

    def keys_count(self, dict_to_check, dict_reference: dict) -> Tuple:
        """Yields the number of keys in each dictionary."""
        yield len(dict_reference), len(dict_to_check)

    def keys_identity(self, dict_to_check, dict_reference: dict) -> Tuple:
        """Cross-checks the identity of a key across two dictionaries and returns a tuple of the result."""
        for key in dict_to_check:
            yield key in dict_reference, True
        for key in dict_reference:
            yield key in dict_to_check, True

    def nested_values(self, nested: Union[list, dict], reference: Union[list, dict]) -> Tuple:
        """Visit all nested nodes and check them against the reference."""
        if isinstance(nested, list):
            for pos, item in enumerate(nested):
                yield from self.nested_values(item, reference[pos])
        if isinstance(nested, dict):
            for key, value in nested.items():
                if hasattr(value, "__dict__"):
                    yield from self.nested_values(vars(value), reference[key])
                elif isinstance(value, list):
                    yield from self.nested_values(value, reference[key])
                elif isinstance(value, dict):
                    yield from self.nested_values(value, reference[key])
                else:
                    yield value, reference[key]
        else:
            yield nested, reference

    @pytest.mark.parametrize('function', ["keys_count", "keys_identity", "nested_values"])
    def test_from_dict(self, function, code_description, dict_ref):
        code_description.from_dict(dict_ref)
        tested_function = getattr(TestCodeDescription, function)
        for tested, expected in tested_function(self, vars(code_description), dict_ref):
            assert tested == expected

    @pytest.mark.parametrize('case', ["expect2pass", "expect2fail"])
    def test_save(self, case, code_description, dict_ref, yaml_source, dict_clear_ref):
        """CodeDescription writing tests to the file stream."""
        # Setup:
        tested = StringIO()
        expected = StringIO()
        if case == "expect2pass":
            code_description.from_dict(dict_ref)
        elif case == "expect2fail":
            code_description.from_dict(dict_clear_ref)
        code_description.save(YAMLSerializer(tested))
        yaml.dump(dict_ref,
                  stream=expected,
                  default_flow_style=False,
                  sort_keys=False,
                  indent=4,
                  explicit_start=True)
        # Test case:
        assertion = tested.getvalue() == expected.getvalue()
        if case == "expect2pass":
            assert assertion
        elif case == "expect2fail":
            assert not assertion
        # Teardown
        del tested
        del expected

    @pytest.mark.parametrize('function', ["keys_count", "keys_identity", "nested_values"])
    @pytest.mark.parametrize('case', ["vs_dict_ref", "vs_class"])
    def test_load(self, function, case, code_description, dict_ref, yaml_source):
        """Tests the load() method of the CodeDescription class."""
        code_description.load(YAMLSerializer(yaml_source))

        testing_function = getattr(TestCodeDescription, function)

        if case == "vs_class" and function == "nested_values":
            dict_ref = ProjectSettings().code_description
        else:
            pass



    @pytest.mark.parametrize('case', ["expect2pass", "expect2fail"])
    @pytest.mark.parametrize('function', ["keys_count", "keys_identity", "nested_values"])
    def test_clear(self, case, function, code_description, dict_ref, dict_clear_ref):
        """Tests the clear() method of the CodeDescription class."""
        # Setup:
        code_description.from_dict(dict_ref)
        code_description.clear()
        # Reference to testing function:
        testing_function = getattr(TestCodeDescription, function)
        # Test cases:
        if case == "expect2pass":
            # The assertion case expected to pass:
            for tested, expected in testing_function(self, vars(code_description), dict_clear_ref):
                assert tested == expected
        elif case == "expect2fail":
            # The assertion case expected to not pass:
            if function == "keys_identity":
                # Single configuration case for the key identity:
                for old_key, _ in dict_ref.copy().items():
                    dict_ref[(old_key + '_Test')] = dict_ref[old_key]
                    del dict_ref[old_key]
            else:
                dict_ref["test"] = "test"
            for tested, expected in testing_function(self, vars(code_description), dict_ref):
                assert not tested == expected
