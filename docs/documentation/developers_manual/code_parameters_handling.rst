#######################################################################################################################
Code parameters handling
#######################################################################################################################

Introduction
############

iWrap takes advantage of ``parameters handlers`` in order to access and modify code parameters in easy and intuitive manner.
These objects implements a pack of functions allowing iWrap to load, access and validate parameters files.

Parameters handler interface
#######################################################################################################################

Every code parameters handler must implement ``ParametersHandlerInterface`` in order to be usable by iWrap.
``ParametersHandlerInterface`` is located in ``iwrap.settings.code_parameters_handlers`` package.


Parameters handler development guide
#######################################################################################################################

Additional code parameters handler abstract classes provide convenient way to create custom handlers with minimal effort.

**Dictionary-based handlers**

Any custom handler that relies on reading parameters from file and is based on dictionary (e.g. JSON, YAML) can inherit from ``DictBasedHandler`` class.
In this situation developer has to implement methods and one properties listed below:

* ``formats:`` property returning set of parameters formats custom handler is able to handle e.g. ``{json]``
* ``__init__():`` method calling ``super(<CustomHandler>, self).__init__()``
* ``from_dict(self, dict):`` method converting input dictionary to str and saving it under: ``self._parameters_str`` variable
* ``to_dict(self):`` method returning ``self._parameters_str`` as string
* ``validate():`` method validating ``self._parameters_str`` against ``self._schema_str``. In case of failure it should raise an Exception

**File-reading handlers**

Custom handlers not relying on dictionaries, but still being read from file should inherit from ``GenericHandler``.
In this situation developer has to implement methods and one properties listed below:

* ``formats:`` property returning set of parameters formats custom handler is able to handle e.g. ``{xml]``
* ``__init__():`` method calling ``super(<CustomHandler>, self).__init__()``
* ``get_parameter(self, path_to_node: str):`` method returning value of parameter described by ``path_to_node``
* ``set_parameter(self, path_to_node: str, value):`` method setting value of parameter described by ``path_to_node``
* ``validate():`` method validating ``self._parameters_str`` against ``self._schema_str``. In case of failure it should raise an Exception

.. note::


    ``GenericHandler`` implements ``_extract_path_info()`` method that can be useful when dealing with hierarchical paths similar to XPath.

    .. code-block:: python

        def _extract_path_info(self, splitted_path):
            '''
            Args:
                Splitted path [str] - list of strings defining path to target node. Every string may contain array access operator ('()') e.g. ['path(0)','to(1)','node(2)']

            Returns:
                Tuple
                - First element of splitted path with array access operator subtracted
                - Extracted index from first element's array access operator
                - The rest of the splited path, with first element subtracted
            '''

**Fully custom handlers**

If helper abstract classes don't fulfill custom handler needs, developer has to create it's own handler from scratch by implementing ``ParametersHandlerInterface``

* ``formats:`` - property returning set of parameters formats custom handler is able to handle e.g. ``{custom_format]``
* ``schema:`` - property returning schema string used to validate parameters
* ``parameters:`` - property returning parameters string
* ``parameters_path:`` - property returning path to parameters file
* ``parameters_path:`` - property.setter for path to parameters file
* ``__init__(self):`` - init method
* ``initialize(self, default_parameters_path: str, schema_path: str):`` - method used to initialize handler. This is the place where parameters and schema paths are passed
* ``get_parameter(self, path_to_node: str):`` - method returning value of parameter described by ``path_to_node``
* ``set_parameter(self, path_to_node: str, value):`` - method setting value of parameter described by ``path_to_node``
* ``validate(self):`` - method validating parameters against schema. In case of failure it should raise an Exception
* ``restore_default_parameters_path(self):`` - method resetting parameters path to default value set during actor build process

Handler installation
#######################################################################################################################

When custom handler class is finished, it can be introduced into iWrap putting custom handler module into ``iwrap.settings.code_parameters_handlers`` package.
Modification of ``HandlerFactory``'s ``_handlers`` static variable is also necessary:

.. code-block:: python

    # iwrap.settings.code_parameters_handlers.handler_factory
    ...
    from .custom_hanlder_module import CustomHandler

    class HandlerFactory:
        _handlers = {XMLHandler, JsonHandler, FortranNamelistHandler... , CustomHandler}


.. note::
    Because of the way how iWrap actors are generated, any custom handler must be put into ``iwrap.settings.code_parameters_handlers`` package
    and import in ``HandlerFactory`` must be relative.
