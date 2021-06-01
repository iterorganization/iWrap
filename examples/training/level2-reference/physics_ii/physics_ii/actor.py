from iwrap.settings.code_description import Argument

from physics_ii.common.base_classes.base_actor import ActorBaseClass
from physics_ii.code_parameters import CodeParameters

class PhysicsIIActor (ActorBaseClass):

    CODE_NAME = 'physics_ii'

    def __init__(self):
        super().__init__(self.CODE_NAME)

        argument = Argument( {'name': 'equilibrium0', 'type': 'equilibrium', 'intent': Argument.IN} )
        self.formal_arguments.append(argument)

        argument = Argument({'name': 'equilibrium1', 'type': 'equilibrium', 'intent': Argument.OUT})
        self.formal_arguments.append(argument)

        self.code_parameters = CodeParameters('input_physics.xml', 'input_physics.xsd')







