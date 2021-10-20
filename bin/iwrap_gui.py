import sys

from iwrap.iwrap_main import main


def mains():
    print('IN MAINS')
    return main(is_commandline_mode=False)


if __name__ == '__main__':
    mains()
