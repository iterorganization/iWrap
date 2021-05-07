
class Argument :
    IN = 'IN'
    OUT = 'OUT'

    def __init__(self, name, type, intent):
        self.name = name # user defined name
        self.type = type # IDS type
        self.intent = intent # (IN/OUT) READ ONLY

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
            + 'Type : ' + self.type + '\n' \
            + 'Intent : ' + self.intent + '\n'
        return str_

