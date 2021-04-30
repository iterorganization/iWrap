from iwrap.settings.platform.platform_settings import PlatformSettings


def startup():
    PlatformSettings().initialize()

    # TODO: initialize generators
    pass


class Engine:
    pass
