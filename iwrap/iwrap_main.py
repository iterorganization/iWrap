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
    parser = argparse.ArgumentParser( description='iWrap - a modular component generator, used for creating IMAS '
                                                  'actors from physics models.',
                                      epilog='For more information, visit <http://iter.org/>.',
                                      # formatter_class=argparse.ArgumentDefaultsHelpFormatter
                                      )

    generation_group = parser.add_argument_group( 'Actor generation' )
    generation_group.add_argument( '-a', '--actor-name', type=str,
                                   help="user defined name of the actor" )

    generation_group.add_argument( '-t', '--actor-type', type=str,
                                   default='python',
                                   help="type of an actor to be generated" )

    generation_group.add_argument( '-d', '--data-type', type=str,
                                   default='legacy',
                                   help="type of data to be used by the actor" )

    generation_group.add_argument( '-f', '--file', type=argparse.FileType( 'r' ),
                                   help="a path to code/actor description *.yaml file" )

    information_group = parser.add_argument_group( 'Additional information' )
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

    Engine().startup()

    if args.list_actor_types:
        print()
        print( 'ID', '\t' * 3, 'Description' )
        print( '-' * 70 )
        for generator in Engine().registered_generators:
            print(generator.name, '\t\t:\t', generator.description )
        return 0

    if args.list_actor_details:
        print()
        try:
            generator = Engine().get_generator(args.list_actor_details)
        except Exception as exc:
            print(exc)
            return 1

        print('Actor type:', '\t\t:\t', generator.name)
        print('Actor IDS types', '\t:\t', ", ".join(generator.actor_data_types ))
        print('Native code languages', '\t:\t', ", ".join(generator.code_languages))
        print('Native code IDS types', '\t:\t', ", ".join(generator.code_data_types))
        print('Description', '\t\t:\t', generator.description )
        print('-' * 70 )

        return 0
    if args.file:
        with args.file as file:
            load_code_description( file )
            ProjectSettings.get_settings().project_file_path = file.name
    if args.actor_name:
        ProjectSettings.get_settings().actor_description.actor_name = args.actor_name

    if args.actor_type:
        Engine().active_generator = args.actor_type

    if args.gui:
        from iwrap.gui.application import launch_gui
        launch_gui()
        return 0

    if args.file is None:
        print( 'No code description file to proceed with. Nothing to do... Booooring...' )
        return 0

    status = Engine().generate_actor( info_output_stream=sys.stdout )
    if status:
        sys.exit(status)

    return 0


if __name__ == "__main__":
    # GUI
    # main( ['-a', 'my_actor', '-f', '../tests/code_description-01.yaml'], is_commandline_mode=False )

    # main( ['-a', 'physics_ii', '-f', '../examples/level2/physics_ii.yaml'], is_commandline_mode=False )
    # commandline

    main( ['-f', '../examples/cp2ds/cp2ds.yaml'], is_commandline_mode=False )
    #main( ['-a', 'core2dist_mpi', '-f', '../examples/cp2ds-mpi/cp2ds-mpi.yaml'], is_commandline_mode=True )
    # main( ['-h'] )
    # main(['--list-actor-types'])
    # main(is_commandline_mode = False)
