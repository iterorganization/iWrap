import logging


class Argument(  ):
    """The data class containing information about argument of the native code

    Attributes:
        name (`str`): user name of the argument
        type (`str`): type of the IDS (e.g. 'equilibrium')
        intent (`str`): determines if argument is IN or OUT
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    IN = 'IN' # input type of argument
    OUT = 'OUT' # output type of an argument

    def __init__(self, name, type, intent):
        self.name = name
        self.type = type
        self.intent = intent

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
               + 'Type : ' + self.type + '\n' \
               + 'Intent : ' + self.intent + '\n'
        return str_