import argparse
import os
import sys
from typing import List

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

    parser.add_argument( '-a', '--actor-name', type=str, required=is_commandline_mode,
                         help="user defined name of the actor" )

    parser.add_argument( '-t', '--actor-type', type=str,
                         choices=['python', 'kepler'],
                         default = 'python',
                         help="type of an actor to be generated" )

    parser.add_argument( '-d', '--data-type', type=str,
                         choices=['legacy', 'hdc'],
                         default = 'legacy',
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
    print( code_description )


def main(argv: List[str] = sys.argv[1:], is_commandline_mode=True) -> int:
    parser = get_parser( is_commandline_mode )
    args = parser.parse_args( argv )
    args.gui = not is_commandline_mode

    print( 'GUI :', args.gui )
    print( 'CD ', os.getcwd() )
    print( args )

    if args.file is not None:
        with args.file as file:
            load_code_description( file )

    if args.gui:
        from iwrap.gui.application import launch_gui
        launch_gui()
        return 0

    if args.file is None:
        print( 'No code description file to proceed with. Nothing to do... Booooring...' )
        return 0
    return 0


if __name__ == "__main__":
    #main(['-f', '../tests/code_description-01.yaml' ], is_commandline_mode=False)
    #main( ['-h'] )
    main(is_commandline_mode = False)
