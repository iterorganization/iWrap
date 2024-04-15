#######################################################################################################################
Configuration of iWrap
#######################################################################################################################


.. toctree::
   :numbered:

Site-specific configuration
#######################################################################################################################

.. _yaml_platform_settings_anchor:

After installing, iWrap requires preparation of configuration containing site-specific default values.
This settings should be provided in YAML format. Configured values include:

*   ``directories:`` - this group gathers info about default directories being used by iWrap or generated actors

    *   ``actor_install_dir:`` - default directory, where generated actors will be installed
    *   ``sandbox_dir:`` - default directory used to keep temporary sandbox directories

*   ``batch_jobs:``  - this group gathers info about default settings of batch jobs

    *    ``runner:`` - a command to be used for submission of batch job (e.g. ``sbatch``, ``srun``)
    *    ``options:`` - default options to be provided to batch submission command

*   ``mpi_jobs:`` - this group gathers info about default settings of MPI jobs

    *    ``runner:`` - a command to be used for submission of MPI job (e.g. ``mpiexec``, ``mpirun``)
    *    ``options:`` - default options to be provided to MPI submission command

*   ``debugger:`` - this group gathers info about default debugger to be used by generated actors

    *   ``cmd:`` - a command to be used to simply launch debugger (e.g. ``totalview``, ``gdb``)
    *   ``attach_cmd:`` - a command to be used to attach debugger to a running actor (within workflow process)


.. note::
 Please note:
   *  Default settings can be overwritten by workflow developer in runtime
      (see  :ref:`actor settings description <actor_settings_anchor>` for details)
   *  Describing default values of ``batch_jobs``, ``mpi_jobs`` and ``debuggers``
      (see :ref:`tags usage within options <command_tag_usage_anchor>` for details)



Picking up a proper configuration file
#######################################################################################################################
A configuration file has to be put in ``iwrap/resources/config`` directory. The file name has to be
of format ``config_<HOST_NAME>.yaml`` (e.g. ``config_ITER.CI.yaml``).
To find a proper configuration file, iWrap tries to match a fully qualified domain name (FQDN)
of the current host to the arbitrary ``<HOST_NAME>`` (e.g.: ``ITER.CI``) to build the path to the configuration file.
If domain name cannot be match, the default configuration file is used.


Example of configuration file
#######################################################################################################################

.. code-block:: yaml

     platform_settings:
        directories:
            actor_install_dir: $ITMWORK/IWRAP_ACTORS
            sandbox_dir: $ITMWORK/IWRAP_SANDBOX

        batch_jobs:
            runner: sbatch
            options: --nodes=${batch_nodes} --wait --wrap=

        mpi_jobs:
            runner: mpiexec
            options: -np ${mpi_processes}

        debugger:
            cmd: totalview
            attach_cmd:
                totalview
                    -e 'dset VERBOSE warning'
                    -e 'dset TV::dll_read_loader_symbols_only *'
                    -e 'dset TV::GUI::pop_at_breakpoint true'
                    -e 'dattach python ${process_id}'
                    -e 'dbreak -pending ${init_sbrt_name}'
                    -e 'dbreak -pending ${main_sbrt_name}'
                    -e 'dbreak -pending ${finish_sbrt_name}'
                    -e 'puts  \"\\n\\nTotalView attached to a running Python process.\\n\"'
                    -e 'puts  \"Press any key to continue!\n\"'
                    -e 'puts  \"WARNING:\\tRestarting or killing debugged process will close the workflow!\"'

...
