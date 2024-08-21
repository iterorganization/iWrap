#######################################################################################################################
Actor and Code Description Backward Compatibility
#######################################################################################################################

Introduction
############

Compatibility between YAML versions allows users to work seamlessly even when using outdated code descriptions. 
This mechanism leverages predefined macros, known as `mappings`, to update existing code descriptions to their 
newer versions.

Mappings
############

Mappings are a set of instructions that define how to transition from one version of a code description to another. 
They are based on four simple commands: ``move``, ``delete``, ``set``, and ``add``. These mappings are stored in the 
``iwrap/settings/compatibility/mappings.py`` file (or the ``iwrap.settings.compatibility.mappings`` module) as a list 
of dictionaries, each representing a command and its components, for example:

.. code-block:: python

    mappings = [
    # Add data_dictionary_compliant
    {'command':'add',
     'target':'code_description/implementation/data_dictionary_compliant',
     'value':'$SYS_VAR("IMAS_VERSION")',
     'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") is None' },
    ...
    ]

Keys
==========================================================================================

- ``command`` - The actual action to be taken, such as ``move``, ``delete``, ``set``, or ``add``.
- ``source`` - Path to the node to be used as the source for the ``move`` command.
- ``target`` - Path to the node to be affected by ``move``, ``delete``, ``set``, or ``add`` commands.
- ``condition`` - Python code containing a condition to be evaluated. See the **Conditions** section for more details.
- ``actions`` - A set of mappings ([mapping]) to be executed sequentially under the same condition. Can be used only 
  standalone or with the ``condition`` keyword. See the **Mappings Grouping** section for more details.

Command Meanings
==========================================================================================

- ``add`` - Adds a node if it does not exist and sets its value.
- ``set`` - Sets the value of a node. If the node does not exist, the value will not be set.
- ``move`` - Moves a node from one place to another. Can utilize filters.
- ``delete`` - Deletes a node. Can utilize filters.

Command Syntax
==========================================================================================

Mapping commands have different numbers of arguments, and not all of them can use filters.
The syntax of commands is as follows:

- ``command:add target: [value:] [condition:]``
- ``command:set target: value: [condition:]``
- ``command:move source: target: [condition:] [filter:]``
- ``command:delete target: [condition:] [filter:]``

Arguments surrounded by ``[]`` are optional.

Conditions
==========================================================================================

Conditions are used to prevent command execution if certain requirements are not met.
Conditions are written in Python code but can also use functions such as ``$TYPE_OF()``, ``$VALUE_OF()``, ``$SYS_VAR()``, 
and keywords like ``$SOURCE`` and ``$TARGET``.

**Condition Functions and Keywords**

- **Functions**

  Condition functions extract specific values from the code description or the system environment.

    - ``$TYPE_OF()`` - Returns the type of a code description YAML node, e.g., ``$TYPE_OF("code_description/implementation/data_dictionary_compliant")``.
    - ``$VALUE_OF()`` - Returns the value of a code description YAML node, e.g., ``$VALUE_OF("code_description/implementation/data_dictionary_compliant")``.
    - ``$SYS_VAR()`` - Returns the value of a system environment variable, e.g., ``$SYS_VAR("IMAS_VERSION")``.

- **Keywords**

  Keywords are used to exclude nodes from being affected by a command. Most keyword uses relate to code description arrays 
  and exclude their elements.

    - The keyword ``$SOURCE`` can be used in the ``move`` command, e.g.:

    .. code-block:: python

        # Move only nodes having the child node `intent` with value `IN`
        {'command':'move', 'source':'old/location', 'target':'new/location', 'filter':'$SOURCE["intent"]=="IN"'}

    This command will move only the ``old/location`` array elements that have ``old/location/intent`` value equal to ``"IN"``.

    - The keyword ``$TARGET`` can be used in the ``delete`` command, e.g.:

    .. code-block:: python

        # Delete only nodes having the child node `intent` with value `IN`
        {'command':'delete', 'target':'old/location', 'filter':'$TARGET["intent"]=="IN"'}

    This command will delete only the ``old/location`` array elements that have ``old/location/intent`` value equal to ``"IN"``.

**Condition Example**

Let's assume we want to add a node only if the node ``code_description/iWrap_version`` has a value lower than 1. 
The mapping appropriate to this task would look like the example below:

.. code-block:: python

    {'command':'add', 'target':'some/new/node', 'condition':'$VALUE_OF("code_description/iWrap_version") < 1'}

    # One may also use any other Python expression
    #{'command':'add', 'target':'some/new/node', 'condition':'$TYPE_OF("code_description/iWrap_version") is None'}
    #{'command':'add', 'target':'some/new/node', 'condition':'$VALUE_OF("code_description/iWrap_version") < 1 and $VALUE_OF("code_description/iWrap_version") > 0.5'}
    #{'command':'add', 'target':'some/new/node', 'condition':'$SYS_VAR("IMAS_VERSION") == "3.39.0"'}

Mappings Grouping
==========================================================================================

Mappings may be grouped under a single condition to execute them in sequence. Group keywords are ``condition`` and ``actions``.

.. code-block:: python

    {
        'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") > 3.37.0',
        'actions':[
            {'command':'add', 'target':'new/node'},
            {'command':'add', 'target':'new/another_node'},
            {'command':'add', 'target':'new/third_node'}
        ]
    }

Examples
==========================================================================================

.. code-block:: python

    mappings = [

    # New empty node
    {'command':'add', 'target':'new/node'},

    # New node with value 'new_value'
    {'command':'add', 'target':'new/node', 'value':'new_value'},

    # Set new value of pre-existing node
    {'command':'set', 'target':'new/node', 'value':'new_value'},

    # Move node
    {'command':'move', 'source':'node/location', 'target':'node/target/location'},

    # Move only nodes having the child node `intent` with value `IN`
    {'command':'move', 'source':'old/location', 'target':'new/location', 'condition':'$SOURCE["intent"]=="IN"'},

    # The same but with different syntax
    {'command':'move', 'source':'old/location', 'target':'new/location', 'condition':'$SOURCE.get("intent")=="IN"'},

    # Delete node
    {'command':'delete', 'target':'useless/node'},

    # Delete only nodes having the child node `intent` with value `IN`
    {'command':'delete', 'target':'useless/node', 'condition':'$SOURCE["intent"]=="IN"'},

    # Real mappings used in iWrap

    # Add data_dictionary_compliant
    {'command':'add',
     'target':'code_description/implementation/data_dictionary_compliant',
     'value':'$SYS_VAR("IMAS_VERSION")',
     'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") is None' },

    # Move function names into code_description/implementaton/subroutines/<subroutine>/name = <name>
    {'command':'move',
     'source':'code_description/implementation/subroutines/main',
     'target':'code_description/implementation/subroutines/main/name',
     'condition': '$TYPE_OF("code_description/implementation/subroutines/main") == str'},
    {'command':'move',
     'source':'code_description/implementation/subroutines/init',
     'target':'code_description/implementation/subroutines/init/name',
     'condition': '$TYPE_OF("code_description/implementation/subroutines/init") == str'},
    {'command':'move',
     'source':'code_description/implementation/subroutines/finalize',
     'target':'code_description/implementation/subroutines/finalize/name',
     'condition': '$TYPE_OF("code_description/implementation/subroutines/finalize") == str'},

    # Move arguments into code_description/implementaton/subroutines/main/arguments
    {'command':'move',
     'source':'code_description/arguments',
     'target':'code_description/implementation/subroutines/main/arguments',
     'condition':'$TYPE_OF("code_description/arguments") is not None'},

    # Add code_description/implementaton/subroutines/<subroutine>/need_code_parameters
    {'command':'add',
     'target':'code_description/implementation/subroutines/main/need_code_parameters',
     'value':True,
     'condition':'$VALUE_OF("code_description/implementation/code_parameters/parameters") is not None'},
    {'command': 'add',
     'target': 'code_description/implementation/subroutines/init/need_code_parameters',
     'value': True,
     'condition': '$VALUE_OF("code_description/implementation/code_parameters/parameters") is not None '
                  'and bool($VALUE_OF("code_description/implementation/subroutines/init"))'}
    ]

Script `iwrap-yaml-update`
######################################

The `iwrap-yaml-update` script leverages the compatibility mechanism to update outdated code descriptions to newer versions 
and save them to the filesystem. This tool is particularly convenient when working with a large number of files. 
The script is located inside iWrap's `bin` directory.

**Syntax**

.. code-block:: console

   usage: iwrap-yaml-update [-h] -f FILE [-o OUTPUT] [-i]

    options:
    -h, --help                  show this help message and exit
    -f FILE, --file FILE        Input filename
    -o OUTPUT, --output OUTPUT  Output filename
    -i, --in-place              Input is renamed as <input>_old. Output is saved under input's filename

| If the output file is not specified, the result will be dumped into the console.
| The output filename and ``--in-place`` flag cannot be used together.

**Examples of Usage**

.. code-block:: console

    # Update code description and print it to the console
    iwrap-yaml-update -f outdated.yaml

    # Update code description and save it as updated.yaml
    iwrap-yaml-update -f outdated.yaml -o updated.yaml

    # Update code description and save it as code_description.yaml.
    # The old code description will be saved as code_description_old.yaml
    iwrap-yaml-update -f code_description.yaml --in-place
