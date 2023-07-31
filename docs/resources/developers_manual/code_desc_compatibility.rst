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
==========================================================================================

Mappings are set of instructions how to move from one version of code description to another. They are based on 4 simple commands i.e. MOVE, DELETE, SET, ADD.
Mappings used in the process are stored in the file ``iwrap/settings/compatibility/mappings.py`` (or module ``iwrap.settings.compatibility.mappings``) as list of tuples of separated command components e.g.:

.. code-block:: python

    mappings = [
    ('move','old_location','new_location','filter'),
    ('delete','useless_node'),
    ...
    ]

**Syntax:**

.. code-block:: console

    COMMAND ARGUMENT1 [ARGUMENT2] [FILTER]

**Arguments:**

    - `COMMAND` - the actual action to be taken i.e. MOVE, DELETE, SET, ADD,
    - `ARGUMENT1/ARGUMENT2` - path to node to be affected,
    - `FILTER` - python code started with ``$arg1`` expression. It could contain any condition. Since ``$arg1`` refers to yaml path passed as ``ARGUMENT1`` .e.g. ``$arg1["child_node_name"]=='Jerry'``.

**Commands syntax:**

Mapping commands have different number of arguments and not all of them are able to use filters.
The syntax of commands is as follows:
    - `ADD path_to_node [value]`,
    - `SET path_to_node value`,
    - `MOVE path_to_node new_path_to_node [filter]`,
    - `DELETE path_to_node [filter]`.

Argument surrounded by ``[]`` means it is optional.

**Examples:**

.. code-block:: python

    #new empty node
    ADD new/node

    #new node with value 'new_value'
    ADD new/node new_value


    #set new value of pre-existing node
    SET new/node new_value


    #move node
    MOVE old/location new/location

    #move only nodes having child node `intent` with value `IN`
    MOVE old/location new/location $arg1["intent"]=='IN'

    #the same but different syntax
    MOVE old/location new/location $arg1.get("intent")=='IN'


    #delete node
    DELETE useless/node

    #delete only nodes having child node `intent` with value `IN`
    DELETE useless/node $arg1["intent"]=='IN'

Script `iwrap-yaml-update`
==========================================================================================
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