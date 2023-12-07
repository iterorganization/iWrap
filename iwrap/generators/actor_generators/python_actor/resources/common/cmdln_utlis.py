import logging
import string

from .runtime_settings import RuntimeSettings, RunMode, DebugMode

__logger = logging.getLogger( __name__)

def __resolve_batch_tags(cmd: str, runtime_settings: RuntimeSettings):
    batch_settings = runtime_settings.batch
    if not batch_settings:
        return cmd

    batch_settings_dict = vars( batch_settings )
    cmd = string.Template( cmd ).safe_substitute( batch_settings_dict )

    return cmd


def __create_batch_cmd(executable, runtime_settings: RuntimeSettings):
    cmd = []

    if runtime_settings.run_mode != RunMode.BATCH:
        return executable

    batch_settings = runtime_settings.batch

    if batch_settings.batch_runner:
        cmd.append( batch_settings.batch_runner )

        if batch_settings.batch_options:
            cmd.append( batch_settings.batch_options )
    else:
        cmd.append( batch_settings.batch_default_runner )

        if batch_settings.batch_options:
            cmd.append( batch_settings.batch_options )

        if batch_settings.batch_default_options:
            cmd.append( batch_settings.batch_default_options )

    cmd = ' '.join( cmd )
    cmd = string.Template( cmd ).safe_substitute( exec=executable )

    return cmd


def __resolve_mpi_tags(cmd: str, runtime_settings: RuntimeSettings):
    mpi_settings = runtime_settings.mpi
    if not mpi_settings:
        cmd = __resolve_batch_tags( cmd, runtime_settings )
        return cmd

    mpi_settings_dict = vars( mpi_settings )
    cmd = string.Template( cmd ).safe_substitute( mpi_settings_dict )

    cmd = __resolve_batch_tags( cmd, runtime_settings )
    return cmd


def __create_mpi_cmd(executable: str, runtime_settings: RuntimeSettings):
    cmd = []

    mpi_settings = runtime_settings.mpi
    if not mpi_settings:
        return __create_batch_cmd( executable, runtime_settings )

    if mpi_settings.mpi_runner:
        cmd.append( mpi_settings.mpi_runner )

        if mpi_settings.mpi_options:
            cmd.append( mpi_settings.mpi_options )
    else:
        cmd.append( mpi_settings.mpi_default_runner )

        if mpi_settings.mpi_options:
            cmd.append( mpi_settings.mpi_options )

        if mpi_settings.mpi_default_options:
            cmd.append( mpi_settings.mpi_default_options )

    cmd = ' '.join( cmd )
    cmd = string.Template( cmd ).safe_substitute( exec=executable )

    cmd = __create_batch_cmd( cmd, runtime_settings )

    return cmd


def resolve_cmd_tags(full_cmd: str, exec: str, runtime_settings: RuntimeSettings):

    full_cmd = string.Template( full_cmd ).safe_substitute( exec=exec )

    full_cmd = __resolve_mpi_tags( full_cmd, runtime_settings )
    return full_cmd

def create_cmd(runtime_settings: RuntimeSettings):
    cmd = []

    if runtime_settings.debug_mode != DebugMode.NONE:
        if runtime_settings.debugger.debugger_cmd:
            debugger_cmd = runtime_settings.debugger.debugger_cmd
        else:
            debugger_cmd = runtime_settings.debugger.debugger_default_cmd
        cmd.append( debugger_cmd )

    if runtime_settings.exec_options:
        cmd.append( runtime_settings.exec_options )

    cmd.append( '${exec}' )

    cmd = ' '.join( cmd )
    if runtime_settings.debug_mode != DebugMode.NONE:
        __logger.warning( 'While standalone debugging MPI and batch modes are switched off!' )
        return cmd

    cmd = __create_mpi_cmd( cmd, runtime_settings )

    return cmd


def validate_command(self, exec_command: str):
    if ';' in exec_command:
        raise ValueError( 'ERROR: User provided command cannot contain ";" ' )
    if '#' in exec_command:
        raise ValueError( 'ERROR: User provided command cannot contain "#" ' )

    if '&&' in exec_command:
        raise ValueError( 'ERROR: User provided command cannot contain "&&" ' )

    if not '${exec}' in exec_command:
        raise ValueError( 'ERROR: User provided command must contain "${exec}" string ' )