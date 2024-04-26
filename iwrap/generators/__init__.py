import logging
import sys
from abc import ABC, abstractmethod

from packaging.version import Version

API_VERSION: str = "2.0"
""" API_VERSION `str`: Version of iWrap <--> plugins interface. 

The "plugin interface" includes all generators methods used to communicate between iWrap and plugins 
and data structures passed from the iWrap to plugins. Version specifiers 
(https://packaging.python.org/en/latest/specifications/version-specifiers) has to be updated manually after any API change,
following Semantic Versioning schema (https://semver.org/).

Side note: This attribute MUST NOT be reassigned, but Python doesn't offer any (good) mechanism to lock it 
"""


class AbstractGenerator( ABC ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    COMPLIANT_API = '0.0'
    """iWrap <-> plugin API version compatible with this plugin.
    
    Value: API version specifier (`str`) (https://packaging.python.org/en/latest/specifications/version-specifiers)
    This attribute should be specified in all implementation classes.
    
    Side note: It be defined as an abstract class property but Python offers such feature
    since version 3.9 (introduced) to 3.13 (deprecated) only.
    """

    @classmethod
    def check_api_compliance(cls) -> None:
        """ Method to check iWrap <--> plugins versions compatibility.

        The method checks the API version currently handled by iWrap (Mi.mi) against declared
        compliant version handled by the plugin (Mp.mp) and detects INCOMPATIBILITY if:
        + The plugin doesn't implement versioning API
        + The major versions differs (Mp != Mi)
        + The plugin API version is newer than iWrap one (Mp.mp > Mi.mi), i.e.: plugin may use changes
           not yet available in the current

        The COMPATIBILITY is assumed only if:
        + The major versions are equal (Mp == Mi) and
        + The plugin API version is older or equal than iWrap one (Mp.mp <= Mi.mi)

        This method can be overwritten in generator implementation classes to better address issues
        related with given plugin compatibility

        """

        api_version: str = API_VERSION
        plugin_name = cls.__module__ +  '.' + cls.__qualname__
        error_msg = f'Plug-in: \"{plugin_name}" ' \
                    'is not compatible with the current iWrap version and cannot be loaded!'

        compliant_api: Version = Version(cls.COMPLIANT_API)
        current_api: Version = Version(api_version)

        if cls.COMPLIANT_API is AbstractGenerator.COMPLIANT_API:
            # attribute was not redefined in subclasses
            reason = "Plugin compatibility cannot be checked. The plugin doesn't implement versioning API."
            raise RuntimeWarning(f'{error_msg}\n\tReason: {reason}')

        if compliant_api.major != current_api.major:
            reason = f"The iWrap and plugin API major versions differs ({current_api} vs {compliant_api})"
            raise RuntimeWarning(f'{error_msg}\n\tReason: {reason}')

        if compliant_api.minor > current_api.minor:
            reason = f"The iWrap API version ({current_api}) is older than API required by the plugin ({compliant_api})"
            raise RuntimeWarning(f'{error_msg}\n\tReason: {reason}')

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    @abstractmethod
    def initialize(self, actor_settings: dict = None):
        """
        Generator initialisation.
        The method is usually used for initialization of the generator environment

        Args:
            actor_settings (`dict`) - complete actor description
        """
        ...

    def validate(self, actor_settings: dict = None):
        """
        Validation of the actor description
        The method checks if an actor description contains all necessary information and if all provided data are correct

        Args:
            actor_settings (`dict`) - complete actor description
        """
        ...

    @abstractmethod
    def generate(self, actor_settings: dict = None):
        """
        Actor files generation.
        Generation of all essential files of from templates

        Args:
            actor_settings (`dict`) - complete actor description
        """
        ...

    @abstractmethod
    def build(self, actor_settings: dict = None):
        """
        Build of the generated actor layer
        The method is used to build an actor

        Args:
            actor_settings (`dict`) - complete actor description
        """
        ...

    @abstractmethod
    def install(self, actor_settings: dict = None):
        """
        Actor installation
        If the actor is generated / built in a temporary directory, this method copies all necessary files in a target/install directory

        Args:
            actor_settings (`dict`) - complete actor description
        """
        ...

    @abstractmethod
    def cleanup(self, actor_settings: dict = None):
        """
        Clean up after the actor layer creation/build process
        The method removes all temporary files, directories, etc...

        Args:
            actor_settings (`dict`) - complete actor description
        """
        ...
