import logging
import sys
from abc import ABC, abstractmethod


class AbstractGenerator( ABC ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

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
