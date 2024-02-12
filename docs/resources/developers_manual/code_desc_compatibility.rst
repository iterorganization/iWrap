#######################################################################################################################
Actor and code description backward compatibility
#######################################################################################################################

############

.. toctree::

Introduction
############

Compatibility between YAML versions allows users to work seamlessly even if they use outdated code description.
This mechanism takes advantage of pre-defined macros named `mappings` to update existing code description to it's newer version.

Mappings
############

Mappings are set of instructions how to move from one version of code description to another. They are based on 4 simple commands i.e. ``move``, ``delete``, ``set``, ``add``.
Mappings used in the process are stored in the file ``iwrap/settings/compatibility/mappings.py`` (or module ``iwrap.settings.compatibility.mappings``) as list of dictionaries of separated command components e.g.:

.. code-block:: python

    mappings = [
    #add data_dictionary_compliant
    {'command':'add',
     'target':'code_description/implementation/data_dictionary_compliant',
     'value':'$SYS_VAR("IMAS_VERSION")',
     'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") is None' },
    ...
    ]


Keys
==========================================================================================

    - `command` - the actual action to be taken i.e. ``move``, ``delete``, ``set``, ``add``,
    - `source` - path to node to be used as source for ``move`` command,
    - `target` - path to node to be affected by ``move``, ``delete``, ``set`` and ``set`` commands,
    - `condition` - python code containing condition to be evaluated. See **Conditions** section for more details.
    - `actions` - set of mappings ([mapping]) to be executed in sequence under the same condition. Can be used only standalone, or with ``condition`` keyword. See **Mappings grouping** section for more details.

Commands meaning
==========================================================================================

    - ``add`` - adds node if not exists and sets it's value
    - ``set`` - sets value of node. If node doesn't exists, value won't be set.
    - ``move`` - moves node from one place to another. Takes advantage of filters.
    - ``delete`` - deletes node. Takes advantage of filters.

Commands syntax
==========================================================================================

Mapping commands have different number of arguments and not all of them are able to use filters.
The syntax of commands is as follows:
    - ``command:add target: [value:] [condition:]``,
    - ``command:set target: value: [condition:]``,
    - ``command:move source: target: [condition:] [filter:]``,
    - ``command:delete target: [condition:] [filter:]``.

Argument surrounded by ``[]`` means it is optional.

Conditions
==========================================================================================

Conditions are used to stop command execution if some requirements aren't met.
Conditions are written in python code, but in addition they can use functions: ``$TYPE_OF()``, ``$VALUE_OF()``, ``$SYS_VAR()`` and ``$SOURCE``, ``$TARGET`` keywords.

    **Condition functions and keywords**

        **Functions**

        Condition functions are used to extract specific values from code description and system environment.

            - ``$TYPE_OF()`` - returns type of code description yaml node e.g. ``$TYPE_OF("code_description/implementation/data_dictionary_compliant")``.
            - ``$VALUE_OF()`` - returns value of code description yaml node e.g. ``$VALUE_OF("code_description/implementation/data_dictionary_compliant")``.
            - ``$SYS_VAR()`` - returns value of system environment variable e.g. ``$SYS_VAR("IMAS_VERSION")``.

        **Keywords**

        Keywords are used to exclude nodes from being affected by command.
        Majority of keywords uses is related to code description arrays and excludes their elements.
        - Keyword ``$SOURCE`` can be used in ``move`` command e.g.:

        .. code-block:: python

            #move only nodes having child node `intent` with value `IN`
            {'command':'move', 'source':'old/location', 'target':'new/location', 'filter':'$SOURCE["intent"]=="IN"'}

        This command will move only ``old/location`` array elements that have ``old/location/intent`` value equal to ``"IN"``.

        - Keyword ``$TARGET`` can be used in ``delete`` command e.g.:

        .. code-block:: python

            #delete only nodes having child node `intent` with value `IN`
            {'command':'delete', 'target':'old/location', 'filter':'$TARGET["intent"]=="IN"'}

        This command will delete only ``old/location`` array elements that have ``old/location/intent`` value equal to ``"IN"``.


    **Condition example**

        Let's assume we want to add some node only if node ``code_description/iWrap_version`` has value lower than 1.
        Mapping appropriate to this task will look like one below:

            .. code-block:: python

                {'command':'add', 'target':'some/new/node', 'condition':'$VALUE_OF("code_description/iWrap_version") < 1'}

                #one may also use any other python expression
                #{'command':'add', 'target':'some/new/node', 'condition':'$TYPE_OF("code_description/iWrap_version") is None'}
                #{'command':'add', 'target':'some/new/node', 'condition':'$VALUE_OF("code_description/iWrap_version") < 1 and $VALUE_OF("code_description/iWrap_version") > 0.5'}
                #{'command':'add', 'target':'some/new/node', 'condition':'$SYS_VAR("IMAS_VERSION") == "3.39.0"'}

Mappings grouping
==========================================================================================

Mappings may be grouped under single condition to execute them in sequence.
Group keywords are ``condition`` and ``actions``.

.. code-block:: python

    {
        'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") > 3.37.0'
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

    #new empty node
    {'command':'add', 'target':'new/node'}

    #new node with value 'new_value'
    {'command':'add', 'target':'new/node', 'value':'new_value'}


    #set new value of pre-existing node
    {'command':'set', 'target':'new/node', 'value':'new_value'}


    #move node
    {'command':'move', 'source':'node/location', 'target':'node/target/location'}

    #move only nodes having child node `intent` with value `IN`
    {'command':'move', 'source':'old/location', 'target':'new/location', 'condition':'$SOURCE["intent"]=="IN"'}

    #the same but different syntax
    {'command':'move', 'source':'old/location', 'target':'new/location', 'condition':'$SOURCE.get("intent")=="IN"'}


    #delete node
    {'command':'delete', 'target':'useless/node'}

    #delete only nodes having child node `intent` with value `IN`
    {'command':'delete', 'target':'useless/node', 'condition':'$SOURCE["intent"]=="IN"'}


    #real mappings used in iWrap

    #add data_dictionary_compliant
    {'command':'add',
     'target':'code_description/implementation/data_dictionary_compliant',
     'value':'$SYS_VAR("IMAS_VERSION")',
     'condition':'$VALUE_OF("code_description/implementation/data_dictionary_compliant") is None' },

    #move function names into code_description/implementaton/subroutines/<subroutine>/name = <name>
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

    #move arguments into code_description/implementaton/subroutines/main/arguments
    {'command':'move',
     'source':'code_description/arguments',
     'target':'code_description/implementation/subroutines/main/arguments',
     'condition':'$TYPE_OF("code_description/arguments") is not None'},

    #add code_description/implementaton/subroutines/<subroutine>/need_code_parameters
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
############
`iwrap-yaml-update` script takes advantage of compatibility mechanism in order to update outdated code description to newer version and save it in filesystem.
This form of the tool is convenient to use especially with large number of files.
The script is located inside `iWrap's` `bin` directory.

**Syntax**

.. code-block:: console

   usage: iwrap-yaml-update [-h] -f FILE [-o OUTPUT] [-i]

    options:
    -h, --help                  show this help message and exit
    -f FILE, --file FILE        Input filename
    -o OUTPUT, --output OUTPUT  Output filename
    -i, --in-place              Input is renamed as <input>_old. Output is saved under input's filename

| If output file is not specified, result will be dumped into console.
| Output filename and ``--in-place`` flag cannot be used together.

**Examples of usage**

.. code-block:: console

    #update code description and print it into console
    iwrap-yaml-update -f outdated.yaml

    #update code description and save it as updated.yaml
    iwrap-yaml-update -f outdated.yaml -o updated.yaml

    #update code description and save it as code_description.yaml.
    #Old code description will be saved as code_description_old.yaml
    iwrap-yaml-update -f code_description.yaml --in-place