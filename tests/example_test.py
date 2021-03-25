from tests.example import example, another_example


def test_example() -> None:
    """Testing example method"""
    assert example() == 1


def test_another_example() -> None:
    """Testing another example method"""
    assert another_example() == "Test"


def test_always_passes():
    """Always passes test"""
    assert True


def test_always_fails():
    assert False
