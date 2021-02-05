
class Argument :
    IN = 'IN'
    OUT = 'OUT'

    def __init__(self, name, arg_type, sub_type, intent):
        self.name = name # (INT, DOUBLE, STRING, COMPLEX, IDS) READ ONLY
        self.type = arg_type # (INT, DOUBLE, STRING, COMPLEX, IDS) READ ONLY
        self.sub_type = sub_type  #'equilibrium'  for IDSes only READ ONLY
        self.intent = intent # (IN/OUT) READ ONLY

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
            + 'Type : ' + self.type + '\n' \
            + 'Sub-type : ' + self.sub_type + '\n' \
            + 'Intent : ' + self.intent + '\n'
        return str_

