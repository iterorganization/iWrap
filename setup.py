# pylint: disable=wrong-import-position
# This file is part of iWrap.
# You should have received iWrap LICENSE file with this project.
import site

# Allow importing local files, see https://snarky.ca/what-the-heck-is-pyproject-toml/
import sys

# Import other stdlib packages
from itertools import chain
from pathlib import Path

# Use setuptools to build packages. Advised to import setuptools before distutils
import tomli
from setuptools import setup
from setuptools.config import read_configuration

from distutils.text_file import TextFile as DistTextFile

# Workaround for https://github.com/pypa/pip/issues/7953
# Cannot install into user site directory with editable source
site.ENABLE_USER_SITE = "--user" in sys.argv[1:]

# We need to know where we are for many things
this_file = Path(__file__)
this_dir = this_file.parent.resolve()

package_name = "iwrap"

# Now that the environment is defined, import the rest of the needed packages
# setup.cfg as read by setuptools
setup_cfg = this_dir / "setup.cfg"
assert setup_cfg.is_file()
conf_dict = read_configuration(setup_cfg)

# Also read the toml for later use
pyproject_toml = this_dir / "pyproject.toml"
assert pyproject_toml.is_file()
pyproject_text = pyproject_toml.read_text()
pyproject_data = tomli.loads(pyproject_text)

optional_reqs = {}
for req in ["core"]:
    optional_reqs[req] = DistTextFile(this_dir / f"requirements_{req}.txt").readlines()
install_requires = optional_reqs.pop("core")
# collect all optional dependencies in a "all" target
optional_reqs["all"] = list(chain(*optional_reqs.values()))

if __name__ == "__main__":
    # Legacy setuptools support, e.g. `python setup.py something`
    # See [PEP-0517](https://www.python.org/dev/peps/pep-0517/) and
    # [setuptools docs](https://setuptools.readthedocs.io/en/latest/userguide/quickstart.html#basic-use)
    # Rough logic setuptools_scm
    # See https://pypi.org/project/setuptools-scm/
    # For allowed version strings, see https://packaging.python.org/specifications/core-metadata/ for allow version strings

    setup(
        #use_scm_version={
        #    "fallback_version": os.getenv("IMASPY_VERSION", "0.0.0"),
        #},
        setup_requires=pyproject_data["build-system"]["requires"],
        install_requires=install_requires,
        extras_require=optional_reqs,
    )

