import sys

from iwrap.iwrap_main import main


def gui():
    return main(is_commandline_mode=False)


def cmd_line():
    return main()


if __name__ == '__main__':
    gui()
