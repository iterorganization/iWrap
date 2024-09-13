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
inherit from the ``DictBasedHandler`` class. In this case, the developer must implement the following methods and property:

* ``formats`` - A property returning a set of parameter formats that the custom handler can handle, e.g., ``{json}``.
* ``__init__()`` - A method calling ``super(<CustomHandler>, self).__init__()``.
* ``from_dict(self, dict)`` - A method converting the input dictionary to a string and saving it in the ``self._parameters_str`` variable.
* ``to_dict(self)`` - A method returning ``self._parameters_str`` as a string.
* ``validate()`` - A method validating ``self._parameters_str`` against ``self._schema_str``. In case of failure, it should raise an exception.

**File-reading Handlers**

Custom handlers that do not rely on dictionaries but still read from a file should inherit from ``GenericHandler``.
In this case, the developer must implement the following methods and property:

* ``formats`` - A property returning a set of parameter formats that the custom handler can handle, e.g., ``{xml}``.
* ``__init__()`` - A method calling ``super(<CustomHandler>, self).__init__()``.
* ``get_parameter(self, path_to_node: str)`` - A method returning the value of the parameter described by ``path_to_node``.
* ``set_parameter(self, path_to_node: str, value)`` - A method setting the value of the parameter described by ``path_to_node``.
* ``validate()`` - A method validating ``self._parameters_str`` against ``self._schema_str``. In case of failure, it should raise an exception.

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

* ``formats`` - A property returning a set of parameter formats that the custom handler can handle, e.g., ``{custom_format}``.
* ``schema`` - A property returning the schema string used to validate parameters.
* ``parameters`` - A property returning the parameters string.
* ``parameters_path`` - A property returning the path to the parameters file.
* ``parameters_path`` - A property setter for the path to the parameters file.
* ``__init__(self)`` - Initialization method.
* ``initialize(self, default_parameters_path: str, schema_path: str)`` - A method used to initialize the handler. This is where parameters and schema paths are passed.
* ``get_parameter(self, path_to_node: str)`` - A method returning the value of the parameter described by ``path_to_node``.
* ``set_parameter(self, path_to_node: str, value)`` - A method setting the value of the parameter described by ``path_to_node``.
* ``validate(self)`` - A method validating the parameters against the schema. In case of failure, it should raise an exception.
* ``restore_default_parameters_path(self)`` - A method resetting the parameters path to the default value set during the actor build process.

Handler Installation
#######################################################################################################################

When the custom handler class is complete, it can be integrated into iWrap by placing the custom handler module into 
the ``iwrap.settings.code_parameters_handlers`` package. Additionally, the ``HandlerFactory``'s ``_handlers`` static 
variable must be modified:

.. code-block:: python

    # iwrap.settings.code_parameters_handlers.handler_factory
    ...
    from .custom_handler_module import CustomHandler

    class HandlerFactory:
        _handlers = {XMLHandler, JsonHandler, FortranNamelistHandler, ..., CustomHandler}

.. note::

    Due to the way iWrap actors are generated, any custom handler must be placed in the ``iwrap.settings.code_parameters_handlers`` package, 
    and the import in ``HandlerFactory`` must be relative.
