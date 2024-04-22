import argparse
import os
import sys
from typing import List

from iwrap.settings.project import ProjectSettings


def get_parser(is_commandline_mode: bool) -> argparse.ArgumentParser:
    """

    Returns:

    """
    from iwrap import __version__

    # Management of input arguments
    parser = argparse.ArgumentParser( description='iWrap - a modular IMAS actor generator, used for creating '
                                                  'standardized actors from a code interfaced with IDSs.',
                                      epilog='For more information, visit <https://imas.iter.org/>.',
                                      # formatter_class=argparse.ArgumentDefaultsHelpFormatter
                                      )

    generation_group = parser.add_argument_group( 'Actor generation' )
    generation_group.add_argument( '-a', '--actor-name', type=str,
                                   help="user defined name of the actor" )

    generation_group.add_argument( '-t', '--actor-type', type=str,
                                   help="type of the actor to be generated" )

    generation_group.add_argument( '-d', '--data-type', type=str,
                                   default='legacy',
                                   help="type of IDS data structure to be used by the actor" )

    generation_group.add_argument( '-f', '--file', type=argparse.FileType( 'r' ),
                                   help="path to the code/actor description *.yaml file" )

    generation_group.add_argument( '-i', '--install-dir', type=str, required=False,
                                   help="actor installation directory" )

    information_group = parser.add_argument_group( 'Additional information' )

    information_group.add_argument( '--plugins-api-version',
                                    action='store_true',
                                    help="prints version of iWrap <-> plugins API" )

    information_group.add_argument( '--list-actor-types',
                                    action='store_true',
                                    help="lists registered actor types that can be generated" )

    information_group.add_argument( '--list-actor-details', type=str,  nargs='?', metavar='ACTOR_TYPE',
                                    const='python',
                                    help="lists details of given actor type generator" )

    information_group.add_argument( '-v', '--version', action='version', dest='show_version',
                                    version='%%(prog)s %s' % __version__ )

    # parser.add_argument( "-g", "--gui", action='store_false', help="runs iWrap graphical interface (optional)" )

    return parser


def load_code_description(project_file):
    ProjectSettings.get_settings().clear()
    project_settings = ProjectSettings.get_settings()
    project_settings.load(  project_file  )



def main(argv: List[str] = sys.argv[1:], is_commandline_mode=True) -> int:
    from iwrap.generation_engine.engine import Engine
    parser = get_parser( is_commandline_mode )
    args = parser.parse_args( argv )
    args.gui = not is_commandline_mode

    if args.plugins_api_version:
        import iwrap.generators
        print('iWrap <-> plugins API version:', '\t', iwrap.generators.API_VERSION )
        return 0

    Engine().startup()

    project_settings = ProjectSettings.get_settings()

    if args.list_actor_types:
        print()
        print( 'Id'.center(20),  ':',  'Name'.center(30),  ':',  'Description' )
        print( '-' * 70 )
        for generator in Engine().registered_generators:
            print(generator.type.center(20), ':', generator.name.center(30),  ':',  generator.description )
        return 0

    if args.list_actor_details:
        print()
        try:
            generator = Engine().get_generator(args.list_actor_details)
        except Exception as exc:
            print(exc)
            return 1

        print('Actor type', '\t\t\t\t:\t', generator.type) 
        print('Description', '\t\t\t\t:\t', generator.description )
        print('Supported IDS types (actor)', '\t\t:\t', ", ".join(generator.actor_data_types ))
        print('Supported languages (code/wrapper)', '\t:\t', ", ".join(generator.code_languages))
        print('Supported IDS types (code/wrapper)', '\t:\t', ", ".join(generator.code_data_types))
        print('-' * 80 )

        return 0
    if args.file:
        with args.file as file:
            load_code_description( file )
            project_settings.project_file_path = file.name

    if args.install_dir:
        project_settings.actor_description._install_dir = args.install_dir

    if args.actor_name:
        project_settings.actor_description.actor_name = args.actor_name

    if args.actor_type:
        project_settings.actor_description.actor_type = args.actor_type

    if args.data_type:
        project_settings.actor_description.data_type = args.data_type

    if project_settings.actor_description.actor_type:
        Engine().active_generator = project_settings.actor_description.actor_type

    if args.gui:
        from iwrap.gui.application import launch_gui
        launch_gui()
        return 0

    if args.file is None:
        print( 'No code description file to proceed with. Nothing to do... ' )
        return 0

    status = Engine().generate_actor( info_output_stream=sys.stdout )
    if status:
        sys.exit(status)

    return 0


if __name__ == "__main__":

    actor_name = "code_restart"
    is_commandline_mode = True

    main( ['-a', f'{actor_name}',
           '--actor-type', 'python',
           '--data-type', 'legacy',
           '-f', f'../examples/{actor_name}/{actor_name}.yaml'],
          is_commandline_mode=is_commandline_mode )

    # main( ['-h'] )
    # main(['--list-actor-types'])
    # main(is_commandline_mode = False)
