import logging


class Argument(  ):
    """The data class containing information about argument of the native code

    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    IN = 'IN' # input type of argument
    OUT = 'OUT' # output type of an argument
