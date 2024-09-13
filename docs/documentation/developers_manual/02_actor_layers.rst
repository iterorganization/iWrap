#######################################################################################################################
Actor architecture
#######################################################################################################################

Actor layers
==========================================================================================

iWrap provides developers with a high degree of freedom in designing actors, including their APIs and internal architectures, as well as in determining how the actors are generated. This chapter outlines the general actor architecture, often referencing the iWrap Python Actor architecture. However, it is ultimately up to the developer to decide which layers to generate and how to structure them, based on the actor's purpose, the programming language in use, and other requirements.

.. figure:: /images/actor_layers.png
  :width: 400
  :alt: Layers of the actor architecture


.. note:: Understanding the structure of an actor and the role of its individual layers is crucial for grasping the actor generation process.


Actor API layer
==========================================================================================

The Actor API Layer is the topmost layer in the actor stack. This layer:

* Is the only layer accessible to the user or higher levels of the software stack (e.g., workflow orchestrators).
* Abstracts the complexity of the underlying layers, presenting a simplified interface to the user.
* Defines the actor's logic.
* For simple cases that do not require passing data and control between different languages (such as MUSCLE3 actors), this may be the only layer necessary.
* Provides a well-defined API for:
  * Configuring how the actor will run.
  * Invoking methods of user-provided code from system libraries or binary executables.


.. admonition:: Example

    Below is an example of the API defined by a `Simple Python` type actor (the sources could be found
    :py:class:`iwrap.generators.actor_generators.python_actor.resources.common.definitions.Actor`)

    .. code-block:: python

        class Actor(ABC):

            @abstractmethod
            def initialize(self, *args, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> Union[List[Any], Any]:
                ...

            @abstractmethod
            def run(self, *args) -> Union[List[Any], Any]:
                ...

            @abstractmethod
            def finalize(self, *args) -> Union[List[Any], Any]:
                ...

            @abstractmethod
            def get_state(self) -> str:
                ...

            @abstractmethod
            def set_state(self, state: str) -> None:
                ...

            @abstractmethod
            def get_timestamp(self) -> float:
            ...


Binding layer
==========================================================================================

The Binding Layer provides the mechanism for connecting higher-level layers, typically implemented in a workflow programming language (e.g., Python), with lower-level layers written in other languages (e.g., C++, Fortran). This layer typically handles:

* Invoking methods of the physics model using various binding mechanisms, such as:
  * Python's ctypes module for Python to C/Fortran bindings.
  * Java's JPype library for Java to C/Fortran bindings.
* Launching standalone executables in separate system processes.
* Converting data (such as method arguments) from Python objects to a format understood by the target code language.


.. admonition:: Example

    Example of the Python ``Binder`` class API (the sources could be found in the class
    :py:class:`iwrap.generators.actor_generators.python_actor.resources.common.binder.Binder`)

    .. code-block:: python

        class Binder (ABC):

            @abstractmethod
            def standalone_cmd(self, method_name:str) -> str:
                ...

            @abstractmethod
            def initialize(self, actor) -> None:
                ...

            @abstractmethod
            def finalize(self) -> None:
                ...

            @abstractmethod
            def call_init(self, *input_idses, code_parameters:str):
                ...

            @abstractmethod
            def call_main(self, *input_idses, code_parameters:str):
                ...

            @abstractmethod
            def call_finish(self *input_idses, code_parameters:str):
                ...

            @abstractmethod
            def call_set_state(self, state:str):
                ...

            @abstractmethod
            def call_get_state(self) -> None:
                ...

            @abstractmethod
            def run_standalone(self, ids_list:List[Any], code_parameters:str, exec_command:str, sandbox_dir:str, output_stream) -> None:
                ...


Wrapper layer
==========================================================================================
The Wrapper Layer plays a crucial role in wrapping the code within its library and standalone executables. It works closely with the Binding Layer to manage data passing and conversion between languages. In essence, it 'reverts' the operations performed by the `Binder`: while the Binder converts IDS and other arguments into a format suitable for inter-layer communication, the Wrapper 'rebuilds' IDS objects (this time in the target code language) based on the received data. This layer also handles auxiliary tasks, such as converting data from a format used by language coupling mechanisms (e.g., ``ctypes``, ``iso_c_binding``, etc.) to a format more user-friendly for developers (e.g., converting character arrays to strings).

.. note::

  Due to the static nature of languages like C or Fortran, the wrapper code is typically generated dynamically based on the number and types of arguments passed from the upper layers. Therefore, there is no fixed API to showcase here.


Code layer
==========================================================================================

The Code Layer represents the implementation of the physics model provided by the developer. This code is already interfaced with IMAS IDSes and must have a standardized API to be wrapped by iWrap.
