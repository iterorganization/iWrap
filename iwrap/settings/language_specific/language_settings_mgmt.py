from iwrap.settings.language_specific.fortran_settings import FortranSpecificSettings
from iwrap.settings.project import ProjectSettings


class LanguageSettingsManager:
    _panes_settings = {'Fortran': FortranSpecificSettings(), 'CPP': None, 'Python': None}

    @classmethod
    def set_settings(cls, pane_name):
        dict_settings = ProjectSettings.get_settings().code_description.language_specific
        programming_language = ProjectSettings.get_settings().code_description.programming_language
        if dict_settings is None or programming_language != pane_name:
            cls._panes_settings[pane_name].clear()
        else:
            cls._panes_settings[pane_name].from_dict(dict_settings)

    @classmethod
    def get_settings(cls, pane):
        return cls._panes_settings[pane]
