import imas

from .data_descriptions import IDSDescription
from .generic_storage import GenericIDSStorage

_BACKEND_NAMES = {
    imas.ids_defs.MEMORY_BACKEND: "memory",
    imas.ids_defs.HDF5_BACKEND: "hdf5",
    imas.ids_defs.MDSPLUS_BACKEND: "mdsplus",
    imas.ids_defs.ASCII_BACKEND: "ascii",
}


class LegacyIDSStorage(GenericIDSStorage):
    def __init__(self):
        self.__occ_dict = {}
        self.__db_entry = None
        self.__uri: str = ""
        self.__backend_name: str = ""

    def __get_occurrence(self, ids_name):
        occ = 1 + self.__occ_dict.get(ids_name, -1)
        self.__occ_dict[ids_name] = occ
        return occ

    def __release_occurrence(self, ids_name):
        occ = self.__occ_dict.get(ids_name, 1)
        self.__occ_dict[ids_name] = occ - 1

    def initialize(self, sandbox_dir: str, backend_id: int):
        backend_name = _BACKEND_NAMES.get(backend_id)
        if backend_name is None:
            raise ValueError(
                f"Unsupported backend_id {backend_id}. "
                f"Supported backends: {list(_BACKEND_NAMES.keys())}"
            )

        self.__uri = f"imas:{backend_name}?path={sandbox_dir}"
        self.__backend_name = backend_name

        try:
            self.__db_entry = imas.DBEntry(self.__uri, "w")
        except Exception as e:
            raise RuntimeError(
                f"Error creating the temporary DB:\n"
                f"  uri = {self.__uri}\n"
                f"Original exception: {e}"
            ) from e

    def prepare_data(self, ids_name):
        occurrence = self.__get_occurrence(ids_name)
        return IDSDescription(self.__uri, ids_name, occurrence)

    def save_data(self, ids_description: IDSDescription, legacy_ids):
        self.__db_entry.put(legacy_ids, ids_description.occurrence)

    def sync_for_external_access(self):
        # External standalone processes must own persistent backends while they run.
        if self.__backend_name != "memory" and self.__db_entry is not None:
            self.__db_entry.close()
            self.__db_entry = None

    def read_data(self, ids_description: IDSDescription):
        if self.__db_entry is None:
            self.__db_entry = imas.DBEntry(self.__uri, "a")
        return self.__db_entry.get(ids_description.ids_type, ids_description.occurrence)

    def release_data(self, ids_name):
        self.__release_occurrence(ids_name)

    def finalize(self):
        try:
            self.__db_entry.close()
        except Exception:
            pass
