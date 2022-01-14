import os

IWRAP_DIR = os.path.dirname(os.path.realpath(__file__))


def __get_iwrap_version():

    version = '<UNKNOWN>'

    # trying to get INSTALLED package version
    try:
        import pkg_resources
        version = pkg_resources.get_distribution('iwrap').version
    except:
        pass
    else:
        return version

    # trying to get WORKING revision from GIT
    try:
        import subprocess

        git_cmd = ['git', 'describe', '--abbrev=4', '--dirty']

        proc = subprocess.Popen( git_cmd, cwd=IWRAP_DIR, stdout=subprocess.PIPE )
        sys_out = proc.communicate()[0]

        return_code = proc.wait()

        if not return_code:
            version = sys_out.strip().decode( 'ascii' , errors='replace')
            return version
    except :
        pass

    return version


__version__ = __get_iwrap_version()