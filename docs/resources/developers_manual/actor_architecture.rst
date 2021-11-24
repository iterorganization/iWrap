############################################################
Actor architecture
############################################################
.. contents::
.. sectnum::

Actor layered architecture
#######################################################################################################################

+----------------------------+-------------------------+
| Programming language       |     Layer               |
+============================+=========================+
|                            |  Workflow orchestrator  |
|                            +-------------------------+
| Workflow language          | Actor                   |
+           (Python)         +-------------------------+
|                            | Binder                  |
+----------------------------+-------------------------+
|Physics model language      | Wrapper                 |
| (C++, Fortran)             +-------------------------+
+                            | Native code             |
+----------------------------+-------------------------+


Actor
#######################################################################################################################

.. code-block:: python

 class Actor(ABC):

    @abstractmethod
    def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> None:
        ...

    @abstractmethod
    def run(self, *args)->None:
        ...

    @abstractmethod
    def finalize(self) -> None:
        ...
Binder
#######################################################################################################################

.. code-block:: python

 class Binder (ABC):

    @abstractmethod
    def initialize(self, actor) -> None:
        ...

    @abstractmethod
    def finalize(self) -> None:
        ...

    @abstractmethod
    def call_init(self, code_parameters: str, sandbox_dir: str, debug_mode=False) -> None:
        ...

    @abstractmethod
    def call_main(self, input_idses, code_parameters:str, sandbox_dir:str) -> None:
        ...

    @abstractmethod
    def call_finish(self, sandbox_dir: str) -> None:
        ...

    @abstractmethod
    def run_standalone(self, ids_list:List[Any], code_parameters:str, exec_command:str, sandbox_dir:str, output_stream) -> None:
        ...

Wrapper
#######################################################################################################################


Native code
#######################################################################################################################

This is a link to the RST Overview: :ref:`RST Overview`