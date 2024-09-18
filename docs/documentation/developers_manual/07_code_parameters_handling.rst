#######################################################################################################################
Code Parameters Handling
#######################################################################################################################

Introduction
############

iWrap uses `parameters handlers` to access and modify code parameters in an easy and intuitive manner. These objects 
implement a set of functions that allow iWrap to load, access, and validate parameters files.

Parameters Handler Interface
#######################################################################################################################

Every code parameters handler must implement the ``ParametersHandlerInterface`` to be usable by iWrap. 
The ``ParametersHandlerInterface`` is located in the ``iwrap.settings.code_parameters_handlers`` package.

Parameters Handler Development Guide
#######################################################################################################################

Additional abstract classes for code parameters handlers provide a convenient way to create custom handlers with minimal effort.

**Dictionary-based Handlers**

Any custom handler that relies on reading parameters from a file and is based on a dictionary (e.g., JSON, YAML) can 
inherit from the ``DictBasedHandler`` class. In this case, the developer has to implement the following methods and property:


* :py:attr:`formats<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.formats>` - A property returning a set of parameter formats that the custom handler can handle, e.g., ``{json}``.
* :py:meth:`__init__(self)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.__init__>` - A method calling ``super(<CustomHandler>, self).__init__()``.
* :py:meth:`from_dict(self, dict)<iwrap.settings.code_parameters_handlers.dict_based_handler.DictBasedHandler.from_dict>` - A method converting the input dictionary to a string and saving it in the ``self._parameters_str`` variable.
* :py:meth:`to_dict(self)<iwrap.settings.code_parameters_handlers.dict_based_handler.DictBasedHandler.to_dict>` - A method returning ``self._parameters_str`` as a dictionary.
* :py:meth:`validate(self)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.validate>` - A method validating ``self._parameters_str`` against ``self._schema_str``. In case of failure, it should raise an exception.

**File-reading Handlers**

Custom handlers that do not rely on dictionaries but still read from a file should inherit from ``GenericHandler``.
In this case, the developer must implement the following methods and property:

* :py:attr:`formats<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.formats>` - A property returning a set of parameter formats that the custom handler can handle, e.g., ``{xml}``.
* :py:meth:`__init__(self)<iwrap.settings.code_parameters_handlers.generic_handler.GenericHandler.__init__>` - A method calling ``super(<CustomHandler>, self).__init__()``.
* :py:meth:`get_parameter(self, path_to_node: str)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.get_parameter>` - A method returning the value of the parameter described by ``path_to_node``.
* :py:meth:`set_parameter(self, path_to_node: str, value)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.set_parameter>` - A method setting the value of the parameter described by ``path_to_node``.
* :py:meth:`validate(self)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.validate>` - A method validating ``self._parameters_str`` against ``self._schema_str``. In case of failure, it should raise an exception.

.. note::

    The ``GenericHandler`` class implements the ``_extract_path_info()`` method, which can be useful when dealing with 
    hierarchical paths similar to XPath.

    .. code-block:: python

        def _extract_path_info(self, splitted_path):
            '''
            Args:
                splitted_path [str] - List of strings defining the path to the target node. Each string may contain an array access operator ('()'), e.g., ['path(0)', 'to(1)', 'node(2)'].

            Returns:
                Tuple:
                - The first element of the splitted path with the array access operator subtracted.
                - Extracted index from the first element's array access operator.
                - The rest of the splitted path, with the first element subtracted.
            '''

**Fully Custom Handlers**

If the helper abstract classes do not fulfill the needs of a custom handler, the developer must create the handler from 
scratch by implementing the ``ParametersHandlerInterface``. The following methods and properties must be implemented:

* :py:attr:`formats<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.formats>` - A property returning a set of parameter formats that the custom handler can handle, e.g., ``{custom_format}``.
* :py:attr:`schema<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.schema>` - A property returning the schema string used to validate parameters.
* :py:attr:`parameters<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.parameters>` - A property returning the parameters string.
* :py:attr:`parameters_path<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.parameters_path>` - A property returning the path to the parameters file.
* :py:attr:`parameters_path<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.parameters_path>` - A property setter for the path to the parameters file.
* :py:meth:`__init__<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.__init__>` - Initialization method.
* :py:meth:`initialize<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.initialize>` - A method used to initialize the handler. This is where parameters and schema paths are passed.
* :py:meth:`get_parameter(self, path_to_node: str)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.get_parameter>` - A method returning the value of the parameter described by ``path_to_node``.
* :py:meth:`set_parameter(self, path_to_node: str, value)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.set_parameter>` - A method setting the value of the parameter described by ``path_to_node``.
* :py:meth:`validate(self)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.validate>` - A method validating ``self._parameters_str`` against ``self._schema_str``. In case of failure, it should raise an exception.
* :py:meth:`restore_default_parameters(self)<iwrap.settings.code_parameters_handlers.parameters_handler_interface.ParametersHandlerInterface.restore_default_parameters_path>` - A method resetting the parameters path to the default value set during the actor build process.

Handler Installation
#######################################################################################################################

When the custom handler class is complete, it can be integrated into iWrap by placing the custom handler module into 
the :py:mod:`iwrap.settings.code_parameters_handlers` package. Additionally, the ``HandlerFactory``'s
``_handlers`` static variable must be modified:

.. code-block:: python

    # iwrap.settings.code_parameters_handlers.handler_factory
    ...
    from .custom_handler_module import CustomHandler

    class HandlerFactory:
        _handlers = {XMLHandler, JsonHandler, FortranNamelistHandler, ..., CustomHandler}

.. note::

    Due to the way iWrap actors are generated, any custom handler must be placed
    in the :py:mod:`iwrap.settings.code_parameters_handlers` package,
    and the import in :py:class:`HandlerFactory<iwrap.settings.code_parameters_handlers.handler_factory.HandlerFactory>`
    class must be relative.
