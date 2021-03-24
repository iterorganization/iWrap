from tests.example import example


def test_example() -> None:
    """Testing example method"""
    assert example() == 1


def test_always_passes():
    """Always passes test"""
    assert True


#def test_always_fails():
#    assert False
