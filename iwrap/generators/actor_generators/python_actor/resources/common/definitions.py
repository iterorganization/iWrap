import logging
from abc import ABC, abstractmethod
from typing import Union, List, Any

from .runtime_settings import RuntimeSettings
from .code_parameters import CodeParameters


class Argument(  ):
    """The data class containing information about argument of the native code

    Attributes:
        name (`str`): user name of the argument
        type (`str`): type of the IDS (e.g. 'equilibrium')
        intent (`str`): determines if argument is IN or OUT
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    IN = 'IN' # input type of argument
    OUT = 'OUT' # output type of an argument

    def __init__(self, name, type, intent):
        self.name = name
        self.type = type
        self.intent = intent

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
               + 'Type : ' + self.type + '\n' \
               + 'Intent : ' + self.intent + '\n'
        return str_


class Actor( ABC ):
    """
    This abstract class defines a Simple Python Actor interface

    The class methods corresponds to several states of the actor ‘life’.
    The methods have to be called only in a strict order:
    * Actor creation
    * Configuration of runtime settings
    * Initialisation
    * Calling the main (‘step’) subroutine
    * Finalisation
    """

    @abstractmethod
    def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> None:
        """
        The method performs initialisation actions

        Args:
            runtime_settings (RuntimeSettings, optional): property tells the actor how native code should
            code_parameters (CodeParameters, optional): physics model parameters
        """
        ...

    @abstractmethod
    def run(self, *args) -> Union[List[Any], Any]:
        """
        The main (aka 'step', 'run') method. Could be called in a loop.
        Args:
            args - a list of input IDSes
        Returns:
            A list of output IDSes
        """
        ...

    @abstractmethod
    def finalize(self) -> None:
        """
        The method is used to perform finalisation actions (clean up the environment etc).
        """
        ...

    @abstractmethod
    def get_state(self) -> str:
        """
        A method returning information about internal model state

        Returns:
              An internal state of the code as a string. The iWrap gives a full flexibility to the code developer
              concerning format and content of state description.  It is a kind of a ‘black box’ returned
              from get_state and passed to set_state method during restart, so the only requirement is that
              information returned by get_state is understandable to set_state.

        """
        ...

    @abstractmethod
    def set_state(self, state: str) -> None:
        """
        A method restoring internal state of the model.

        Args:
            state (str) -  An internal state of the code as a string returned by a call of `get_state`
        """
        ...

    @abstractmethod
    def get_timestamp(self) -> float:
        """
        The method allows to obtain currently computed physical time. Such information can help support consistent
        physical time handling throughout the coupled simulation

        Returns:
            Timestamp: currently computed physical time
        """
        ...
