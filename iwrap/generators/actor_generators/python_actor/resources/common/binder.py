from abc import abstractmethod, ABC
from pathlib import Path
from typing import List, Any


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
    def call_init(self, code_parameters:str):
        ...

    @abstractmethod
    def call_main(self, *input_idses, code_parameters:str):
        ...

    @abstractmethod
    def call_finish(self):
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

    @classmethod
    def save_input(cls, method_name, full_arguments_list, code_parameters, sandbox_dir):

        file_path = Path(sandbox_dir, f'{method_name}.in')
        with open( file_path, "wt" ) as file:
            # Save IDS arguments
            file.write( ' Arguments '.center(70, '=') )
            file.write( "\n" )
            file.write( 'Length:' )
            file.write( "\n" )
            file.write( str( len( full_arguments_list ) ) + "\n" )
            for arg in full_arguments_list:
                arg.save( file )

            if code_parameters:
                code_parameters.save(sandbox_dir)

    @classmethod
    def read_output(cls, method_name, status_info, sandbox_dir):

        file_path = Path(sandbox_dir, f'{method_name}.out')
        with open( file_path, "rt", errors='replace' ) as file:
            # Read status info
            status_info.read(file)