import argparse
import os
import sys
from typing import List

from iwrap.generation_engine.engine import Engine
from iwrap.settings.project import ProjectSettings
from iwrap.settings.serialization import YAMLSerializer


def get_parser(is_commandline_mode: bool) -> argparse.ArgumentParser:
    """

    Returns:

    """
    from iwrap import __version__

    # Management of input arguments
    parser = argparse.ArgumentParser( description='iWrap - a modular component generator, used for creating IMAS '
                                                  'actors from physics models.',
                                      epilog='For more information, visit <http://iter.org/>.',
                                      formatter_class=argparse.ArgumentDefaultsHelpFormatter
                                      )

    parser.add_argument( '-a', '--actor-name' , type=str, #required=is_commandline_mode,
                         help="user defined name of the actor" )

    parser.add_argument( '-t', '--actor-type', type=str,
                         choices=['python', 'kepler'],
                         default='python',
                         help="type of an actor to be generated" )

    parser.add_argument( '--list-actor-types',
                         action='store_true',
                         help="lists registered actor types that can be generated" )

    parser.add_argument( '-d', '--data-type', type=str,
                         choices=['legacy', 'hdc'],
                         default='legacy',
                         help="type of data to be used by the actor" )

    parser.add_argument( '-f', '--file', type=argparse.FileType( 'r' ),
                         help="a path to code description *.yaml file" )

    parser.add_argument( '-v', '--version', action='version', dest='show_version',
                         version='%%(prog)s %s' % __version__ )

    # parser.add_argument( "-g", "--gui", action='store_false', help="runs iWrap graphical interface (optional)" )

    return parser


def load_code_description(code_description_file):
    code_description = ProjectSettings.get_settings().code_description
    code_description.load( YAMLSerializer( code_description_file ) )
    file_real_path = os.path.realpath(code_description_file.name)
    ProjectSettings.get_settings().root_dir = os.path.dirname(file_real_path)


def main(argv: List[str] = sys.argv[1:], is_commandline_mode=True) -> int:
    parser = get_parser( is_commandline_mode )
    args = parser.parse_args( argv )
    args.gui = not is_commandline_mode

    Engine().startup()

    if args.list_actor_types:
        for generator in Engine().registered_generators:
            print(  )
            print( 'Actor type:', '\t' ,'ID' , '\t' * 3,'Description')
            print( '-' * 70 )
            print('\t' * 4, generator.name, '\t:\t', generator.description)
        return 0

    if args.actor_name:
        ProjectSettings.get_settings().actor_name = args.actor_name

    if args.actor_type:
        Engine().active_generator = args.actor_type

    if args.file:
        with args.file as file:
            load_code_description( file )

    if args.gui:
        from iwrap.gui.application import launch_gui
        launch_gui()
        return 0

    if args.file is None:
        print( 'No code description file to proceed with. Nothing to do... Booooring...' )
        return 0

    Engine().generate_actor(info_output_stream = sys.stdout)

    return 0


if __name__ == "__main__":
    # GUI
    #main( ['-a', 'my_actor', '-f', '../tests/code_description-01.yaml'], is_commandline_mode=False )

    #main( ['-a', 'physics_ii', '-f', '../examples/level2/physics_ii.yaml'], is_commandline_mode=False )
    # commandline
    main( ['-a', 'core2dist', '-f', '../examples/cp2ds/cp2ds.yaml'], is_commandline_mode=False )
    #main( ['-h'] )
    #main(['--list-actor-types'])
    # main(is_commandline_mode = False)
