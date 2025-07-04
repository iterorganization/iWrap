import importlib
import imas
import numpy as np
import os


def get_actor_class(full_actor_name: str):
    """
    Returns class of actor, which name is passed as argument
    """
    actor_lib = importlib.import_module(f"{full_actor_name}.actor")
    actor_class = getattr(actor_lib, f"{full_actor_name}")

    return actor_class


def get_actor_instance(actor_name=None, code_language=None, parameters_format=None):
    """
    Returns instance of actor, which name is passed as argument
    """

    if os.getenv("CI_RUN"):
        actor_name = os.getenv("ACTOR_NAME")
        code_language = os.getenv("CODE_LANGUAGE")
        parameters_format = os.getenv("PARAMETERS_FORMAT")
        if not actor_name or not code_language or not parameters_format:
            raise RuntimeError("ACTOR_NAME, CODE_LANGUAGE and PARAMETERS_FORMAT env variables must be set when running with CI_RUN flag. ")
    else:
        if not parameters_format:
            parameters_format = "none"
            print("=> [WARNING] parameters_format was not set. Assuming parameters_format = none.")
        if not actor_name or not code_language:
            raise RuntimeError(
                "Workflow parameters are not set."
                " Remember to pass explicitly actor_name and code_language into get_actor_instance() method."
            )

    full_actor_name = f"{actor_name}_{code_language}_{parameters_format}".replace(
        "-", "_"
    )

    return get_actor_class(full_actor_name)()


def get_test_equilibrium():
    """
    Returns an instance of equilibrium IDS used in test workflows
    """
    return create_test_ids("equilibrium", 1, 3)


def get_test_core_profiles():
    """
    Returns an instance of core_profiles IDS used in test workflows
    """
    return create_test_ids("core_profiles", 10, 3)


def get_test_distribution_sources():
    """
    Returns an instance of distribution_sources IDS used in test workflows
    """
    return create_test_ids("distribution_sources", 100, 3)


def create_test_ids(ids_class, seed=1, steps=3):
    """
    Returns an instance IDS filled with test data
    """
    factory = imas.IDSFactory()
    ids = factory.new(ids_class)
    ids.ids_properties.homogeneous_time = imas.ids_defs.IDS_TIME_MODE_HOMOGENEOUS
    ids.time = np.array([float(x) for x in range(seed, seed + steps)])

    return ids
