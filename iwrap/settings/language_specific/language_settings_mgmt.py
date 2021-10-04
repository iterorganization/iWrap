from iwrap.settings.language_specific.fortran_settings import FortranSpecificSettings



class LanguageSettingsManager:
    _language_settings_handlers = {'fortran': FortranSpecificSettings(), 'cpp': FortranSpecificSettings(),
                                   'python': None}

    # TODO: Add dynamic discovery of settings handlers

    @classmethod
    def get_settings_handler(cls, language, values=None):

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
        return cls._language_settings_handlers[language]
