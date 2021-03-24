from iwrap.gui.settings.language_specific_panes.cpp_settings_pane import CppPane
from iwrap.gui.settings.language_specific_panes.fortran_settings_pane import FortranPane
from iwrap.gui.settings.language_specific_panes.python_settings_pane import PythonPane


class LanguagePanesManager():

    _registered_panes = {'Fortran' : FortranPane, 'CPP': CppPane, 'Python': PythonPane}

    @classmethod
    def get_language_pane(cls, language: str) :
        language_pane = cls._registered_panes[language]
        return language_pane


