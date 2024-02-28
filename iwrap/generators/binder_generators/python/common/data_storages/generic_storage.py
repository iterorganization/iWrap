from abc import ABC, abstractmethod

class GenericIDSStorage(ABC):

    @abstractmethod
    def prepare_data(self, ids_name):
        ...

    @abstractmethod
    def save_data(self, ids_name, ids_data):
        ...

    @abstractmethod
    def read_data(self, ids_name, ids_data):
        ...

    def initialize(self, db_name:str, backend_id):
        ...
