.. _iWrap CLI:

#######################################################################################################################
Generation of an actor using iWrap commandline
#######################################################################################################################

Once YAML file is prepared it can be used for generation of an actor using iWrap commandline.

.. code-block:: console

    usage: iwrap [-h] [-a ACTOR_NAME] [-t ACTOR_TYPE] [-d DATA_TYPE] [-f FILE] [-i INSTALL_DIR] [--plugins-api-version]
                 [--list-actor-types] [--list-actor-details [ACTOR_TYPE]] [-v]

    iWrap - a modular IMAS actor generator, used for creating standardized actors from a code interfaced with IDSs.

    options:
      -h, --help            show this help message and exit

    Actor generation:
      -a ACTOR_NAME, --actor-name ACTOR_NAME
                            user defined name of the actor
      -t ACTOR_TYPE, --actor-type ACTOR_TYPE
                            type of the actor to be generated
      -d DATA_TYPE, --data-type DATA_TYPE
                            type of IDS data structure to be used by the actor
      -f FILE, --file FILE  path to the code/actor description *.yaml file
      -i INSTALL_DIR, --install-dir INSTALL_DIR
                            actor installation directory

    Additional information:
      --plugins-api-version
                            prints version of iWrap <-> plugins API
      --list-actor-types    lists registered actor types that can be generated
      --list-actor-details [ACTOR_TYPE]
                            lists details of given actor type generator
      -v, --version         show program's version number and exit

    For more information, visit <https://imas.iter.org/>.

If YAML file contains both code description and actor description parts, no additional switches are required.



.. code-block:: console

                     shell> iwrap -f actor_and_code_descriptions.yaml

If YAML contains only code description, additional information necessary to generate an actor must be provided.
An actor name is mandatory switch in such case, while the other arguments (actor and data type) are optional - if they
are absent, default values are used.

.. code-block:: console

 shell> iwrap -a actor_name -f code_descriptions.yaml

