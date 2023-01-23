import logging
from iwrap.settings.settings.fortran_settings import FortranSpecificSettings


class LanguageSettingsManager:
    """Language settings manager, change language settings object when programming language changed.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    _language_settings_handlers = {'fortran': FortranSpecificSettings(), 'cpp': FortranSpecificSettings()}

    # TODO: Add dynamic discovery of settings handlers

    @classmethod
    def get_settings_handler(cls, language, values=None):
        """ Returns language settings.
        """
        language = language.lower()
        # no special handler
        if language not in cls._language_settings_handlers:
            return values or {}

        language_handler = cls._language_settings_handlers[language]
        language_handler.clear()

        if values and isinstance( values, dict ):
            language_handler.from_dict( values )

        return language_handler

    @classmethod
    def get_settings(cls, language):
        """
        Returns settings for specific language.
        Args:
            language (str): programming language.

        Returns: Settings object.

        """
        return cls._language_settings_handlers.get(language)
