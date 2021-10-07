import logging
from iwrap.gui.settings.language_specific_panes.cpp_settings_pane import CppPane
from iwrap.gui.settings.language_specific_panes.fortran_settings_pane import FortranPane
from iwrap.gui.settings.language_specific_panes.python_settings_pane import PythonPane
from iwrap.gui.settings.language_specific_panes.not_supported_language_settings_pane import NotSupportedLanguagePane

class LanguagePanesManager():
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)


    _registered_panes = {'fortran' : FortranPane, 'cpp': FortranPane, 'python': PythonPane}

    @classmethod
    def get_language_pane(cls, language: str) :
        if language in cls._registered_panes.keys():
            language_pane = cls._registered_panes[language]
        else:
            language_pane = NotSupportedLanguagePane
        return language_pane


