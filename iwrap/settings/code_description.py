import logging
import os
from typing import List, Dict, Any
import yaml

from pathlib import Path
from enum import Enum

from iwrap.common import utils
from iwrap.generation_engine.engine import Engine
from iwrap.settings import SettingsBaseClass
from iwrap.settings.settings.language_settings_mgmt import LanguageSettingsManager
from iwrap.settings.code_parameters_handlers.handler_factory import HandlerFactory
from iwrap.settings.code_parameters_handlers.parameters_handler_interface import ParametersHandlerInterface

class Intent( Enum ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    IN = 'IN'  # input type of argument
    OUT = 'OUT'  # output type of an argument


class Argument( SettingsBaseClass ):
    """The data class containing information about the arguments of the code's API to be wrapped

    Attributes:
        name (`str`): name of the argument
        type (`str`): type of the IDS (e.g. 'equilibrium')
        _intent : determines if the argument is IN or OUT
    """
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    @property
    def intent(self):
        return self._intent.value

    @intent.setter
    def intent(self, value):
        self._intent = Intent[value.upper()]

    def __init__(self, dictionary: dict):
        self.name = dictionary['name']
        self.type = dictionary['type']
        self._intent = None
        self.intent = dictionary['intent']

    def clear(self):
        # The whole list is cleared, so no need to clear particular elements
        pass

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:
        if not self.name:
            raise ValueError( 'Argument name is not set!' )

        if not self.type:
            raise ValueError( 'Argument IDS type is not set!' )

        data_type = kwargs['data_type']
        ids_list = Engine.get_ids_types( data_type )
        if self.type not in ids_list:
            raise ValueError( f'Incorrect IDS type {self.type} of argument {self.name}!' )

        if not self._intent:
            raise ValueError( 'Argument intent is not set!' )

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
        """

        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict(resolve_path, make_relative, project_root_dir)

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
               + 'Type : ' + self.type + '\n' \
               + 'Intent : ' + self.intent + '\n'
        return str_


class Subroutines( SettingsBaseClass ):
    """The data class containing information about the subroutines to be called from the code's library.

    Attributes:
        init (str): Name of the subroutine that can be used to initialize the code (optional)
        main (str): Name of the main subroutine of the code that will be called from the actor (mandatory)
        finalize (str): Name of the subroutine that can be used to finalize the code (optional)
        get_state (str): Name of the subroutine returning the internal state of the code during a checkpoint (optional)
        set_state (str): Name of the subroutine restoring the internal state of the code during a restart (optional)
        get_timestamp (tk.StringVar()): Name of the subroutine providing the time in the simulation (optional)
    """

    def __init__(self):
        # A name of subroutine that could be used to initialise the code (optional)
        # (Please note: must be *exactly the same* as name of called method / subroutine!)
        self.init = Subroutine('init')

        # A name of the main subroutine that will be called from actor (mandatory)
        # (Please note: must be *exactly the same* as name of called method / subroutine!)
        self.main = Subroutine('main', True)

        # A name of subroutine that could be used to finalise the code (optional)
        # (Please note: must be *exactly the same* as name of called method / subroutine!)
        self.finalize = Subroutine('finalize')

        self.get_state = ''

        self.set_state = ''

        self.get_timestamp = ''

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:

        self.init.validate(engine, project_root_dir, **kwargs)
        self.main.validate(engine, project_root_dir, **kwargs)
        self.finalize.validate(engine, project_root_dir, **kwargs)


    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.init = Subroutine('init')
        self.main = Subroutine('main', True)
        self.finalize = Subroutine('finalize')
        self.get_state = ''
        self.set_state = ''
        self.get_timestamp = ''

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict(resolve_path, make_relative, project_root_dir)


class Implementation( SettingsBaseClass ):
    """The data class containing information about the code implementation.

    Attributes:
        root dir (str): root directory
        _programming_language (str): language of in which the code's API is implemented
        data_type (:obj:str): data type handled by the code { 'Legacy IDS', 'HDC IDS'}
        code_path  (str):  path to library (C, CPP), script (Python), etc, containing the code and
            methods/subroutines to be called
        include path (str): path to the module's / header's file defining the code's API signature
        data_dictionary_compliant (str): oldest version of the data dictionary the code is compatible with
        code_parameters (:obj:CodeParameters): code specific parameters
        subroutines (:obj:Subroutines): name of the method/subroutine defined in the code's API
    """
    @property
    def programming_language(self):
        return self._programming_language

    @programming_language.setter
    def programming_language(self, value: str):
        value = None if str( value ).lower() == 'none' or value == '' else value
        self._programming_language =  value.lower() if value else value
        self._master.change_language_specific()

    def __init__(self, master):
        self.root_dir = '.'
        self._programming_language  = None
        self.data_type: str = None
        self.code_path: str = None
        self.include_path = None
        self.data_dictionary_compliant: str = None
        self._master = master
        self.code_parameters: CodeParameters = CodeParameters()
        self.subroutines: Subroutines = Subroutines()

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:

        is_dummy_actor = False if self.programming_language else True

        # programming_language
        if is_dummy_actor:
            engine.validate_programming_language( self.programming_language )

        # data_type
        if not self.data_type:
            raise ValueError( 'Type of data handled by the code is not set!' )
        else:
            engine.validate_code_data_type( self.data_type )

        # code path
        if not is_dummy_actor:
            if not self.code_path:
                raise ValueError( 'Path to the code is not set!' )

            __path = utils.resolve_path( self.code_path, project_root_dir )
            if not Path(__path).exists():
                raise ValueError( 'Path to the code points to not existing location ["' + str( __path ) + '"]' )

        # code parameters
        self.code_parameters.validate( engine, project_root_dir )

        # include path
        if not is_dummy_actor:
            if not self.include_path:
                raise ValueError( 'Path to the include/module file is not set!' )

        if not self.data_dictionary_compliant:
            raise ValueError('Data Dictionary compliant version is not set!')

        # subroutines
        self.subroutines.validate( engine, project_root_dir, **kwargs, data_type = self.data_type )

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.root_dir = '.'
        self.programming_language = None
        self.data_type = None
        self.code_path = None
        self.data_dictionary_compliant = None
        self.subroutines.clear()
        self.code_parameters.clear()

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        if resolve_path:
            # include_path
            __path = utils.resolve_path( self.include_path, project_root_dir )
            ret_dict.update( {'include_path': __path} )

            # code_path
            code_path = self.code_path
            __path = utils.resolve_path( code_path, project_root_dir )
            ret_dict.update( {'code_path': __path} )

        return ret_dict


class CodeParameters( SettingsBaseClass ):
    """The data class containing information about files defining code parameters."""
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        #: A path to file containing the code specific parameters
        self.parameters: str = ''

        #: A path to file containing schema that allows to validate code parameters XML description
        self.schema: str = ''

        #: A format of code parameters (legacy-xml, json, fortran_namelist, xml , ...)
        self.format: str = 'legacy-xml'

    def validate(self, engine: Engine, project_root_dir: str) -> None:
        if self.parameters and not self.schema:
            raise ValueError( 'Parameters schema must be set if parameters file is specified!' )

        # parameters
        if self.parameters:
            __path = utils.resolve_path( self.parameters, project_root_dir )
            if not Path(__path).exists():
                raise ValueError( f'Path to parameters file is invalid! {str( __path )}' )

        # schema
        if self.schema:
            __path = utils.resolve_path( self.schema, project_root_dir )
            if not Path(__path).exists():
                raise ValueError( f'Path to parameters schema file is invalid! {str( __path )}' )

        if self.parameters:
            parameters_handler : ParametersHandlerInterface = HandlerFactory.get_handler(self.format)
            parameters_handler.initialize(utils.resolve_path(self.parameters, project_root_dir), utils.resolve_path(self.schema, project_root_dir))
            parameters_handler.validate()


    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.parameters = ''
        self.schema = ''
        self.format = 'legacy-xml'

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        if resolve_path:
            # parameters
            if self.parameters:
                __path = utils.resolve_path( self.parameters, project_root_dir )
                ret_dict.update( {'parameters': __path} )

            # schema
            if self.schema:
                __path = utils.resolve_path( self.schema, project_root_dir )
                ret_dict.update( {'schema': __path} )

            # format
            if self.format:
                ret_dict.update( {'format': self.format} )

        return ret_dict


class CodeDescription( SettingsBaseClass ):
    """Description of the code used for wrapping the code within an actor.

    Attributes:
        documentation (str): human readable description of the code
        _settings (dict): code settings
        implementation(:obj:`Implementation`): details on the implementation of the code
    """
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    @property
    def settings(self):
        return self._settings

    @settings.setter
    def settings(self, values):
        # language specific settings depends on language chosen
        # if language was not set yet, language specific settings will be set in language property handler
        self._settings = LanguageSettingsManager.get_settings_handler( self.implementation.programming_language, values )

    def __init__(self):
        self.implementation: Implementation = Implementation(self)
        self.documentation: str = None
        self._settings: dict = {}

    def change_language_specific(self):
        """ Update settings when programming language changed.
        """
        if self._settings is not None and isinstance(self._settings, dict):
            self._settings = LanguageSettingsManager.get_settings_handler(self.implementation.programming_language,
                                                                                          self._settings)

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:
        # implementation
        self.implementation.validate(engine, project_root_dir)

        # documentation
        if self.documentation and not isinstance( self.documentation, str ):
            raise ValueError( 'Documentation must be a string (and it is not)!' )

        # settings
        if isinstance(self.settings, SettingsBaseClass):
            self.settings.validate(engine, project_root_dir)  # pylint: disable=no-member

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.documentation = None
        self.implementation.clear()
        self.settings = {}

    def to_dict(self, resolve_path: bool = False,
                make_relative: bool = False,
                project_root_dir: str = None) -> Dict[ str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        return ret_dict

    def save(self, file):
        """Stores code description in a file

        Args:
            file: an object responsible for storing dictionary from file of given format
        """
        code_description_dict = self.to_dict()
        yaml.dump( {'code_description': code_description_dict}, stream=file, default_flow_style=False, sort_keys=False,
                   indent=4, explicit_start=True, explicit_end=True )

    def load(self, file):
        """Loads code description from a file

        Args:
            file: an object responsible for reading dictionary from file of given format
        """
        self.clear()
        dict_read = yaml.load( file, Loader=yaml.Loader )
        if not dict_read:
            raise Exception( "The file being loaded doesn't seem to be a valid YAML" )

        code_description_dict = dict_read.get( 'code_description' )
        if not code_description_dict:
            raise Exception( "The YAML file being loaded doesn't seem to contain valid description of the code" )

        self.from_dict( code_description_dict )

        file_real_path = os.path.realpath( file.name )
        if not self.implementation.root_dir:
            self.implementation.root_dir = os.path.dirname( file_real_path )


class Subroutine(SettingsBaseClass):
    """Data type for Subroutines class.

    Attributes:
        name (str): Name of a subroutine init, main, or finalize.
        need_code_parameters (bool): A boolean for making code
            parameters optional.
        _arguments (list [:obj:`Arguments`]): List of native code in/out arguments.
    """

    @property
    def arguments(self):
        return self._arguments

    @arguments.setter
    def arguments(self, values):
        self._arguments = []
        for value in values or []:
            if not isinstance(value, Argument):
                value = Argument(value)
            self._arguments.append(value)

    def __init__(self, method_type, mandatory: bool = False):
        self.name = None
        self.need_code_parameters = False
        self._arguments = []
        self.__method_type: str = method_type
        self.__mandatory: bool = mandatory


    def clear(self):
        """
        Clears class content, setting default values of class attributes.
        """
        self.name = None
        self.need_code_parameters = False
        self.arguments = []

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:

        # validate correctness of XML
        if not self.name and self.__mandatory:
            raise ValueError( 'Mandatory method name for "' + self.__method_type.upper() + '" is not set!' )

        for argument in self.arguments or []:
            argument.validate(engine, project_root_dir, **kwargs)
