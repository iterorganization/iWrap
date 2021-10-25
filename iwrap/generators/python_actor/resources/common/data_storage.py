from abc import ABC


class AbstractDataStorage(ABC):
    ...


class LegacyIdsStorage(AbstractDataStorage):


    def __create_work_db(self):
        ids_storage = self.runtime_settings.ids_storage
        is_standalone_run = self.is_standalone_run()

        if is_standalone_run and ids_storage.backend is imas.imasdef.MEMORY_BACKEND:
            backend = ids_storage.persistent_backend
        else:
            backend = ids_storage.backend

        db_entry = imas.DBEntry( backend_id=backend,
                                 db_name=ids_storage.db_name,
                                 shot=ids_storage.shot,
                                 run=ids_storage.run )

        db_entry.create()
        return db_entry
