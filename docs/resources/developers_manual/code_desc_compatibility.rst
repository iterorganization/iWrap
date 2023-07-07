#######################################################################################################################
Actor and code description backward compatibility
#######################################################################################################################

############

.. toctree::

Introduction
############
iWrap backward compatibility script was created to allow the user seamless work even if (s)he uses outdated code description.
It takes advantage of pre-defined macros named `mappings` to modify existing .yaml file into it's newer version.

module `iwrap.settings.compatibility`

Mappings
==========================================================================================


File: `iwrap/settings/compatibility/mappings.py` (or module `iwrap.settings.compatibility.mappings`)

Syntax

`COMMAND ARGUMENT1 [ARGUMENT2] [FILTER]`

Arguments

    - COMMAND - the actual action to be taken i.e. MOVE, DELETE, SET
    - ARGUMENT1/ARGUMENT2 - path to node to be affected
    - FILTER - python code started with `arg1` expression. It could contain any condition. Since `arg1` refers to yaml path passed as `ARGUMENT1` it always will be dictionary type .e.g. `arg1["child_node_name"]=='Jerry'`.

Examples

.. code-block:: python

    ADD new/node #new empty node
    ADD new/node new_value #new node with value 'new_value'

    MOVE old/location new/location #move node
    MOVE old/location new/location arg1["intent"]=='IN' #move only nodes having intent==IN
    MOVE old/location new/location arg1.get("intent")=='IN' #the same but different syntax

    DELETE needless/node #delete node
    DELETE needless/node arg1["intent"]=='IN' #delete only intent==IN nodes

Script `iwrap-yaml-update`
==========================================================================================

`iwrap-yaml-update` script is located in iwrap `bin` directory.

.. code-block:: console

   usage: iwrap-yaml-update [-h] -f FILE [-o OUTPUT] [-i]

    options:
    -h, --help                  show this help message and exit
    -f FILE, --file FILE        Input filename
    -o OUTPUT, --output OUTPUT  Output filename
    -i, --in-place              Input is renamed as <input>_old. Output is saved under input's filename


Examples of usage

.. code-block:: console

    iwrap-yaml-update -f outdated.yaml -o updated.yaml
    iwrap-yaml-update -f outdated.yaml --in-place