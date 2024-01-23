import logging
import tkinter as tk
from tkinter import LabelFrame, ttk

from iwrap.gui.generics import IWrapPane
from iwrap.gui.settings.language_specific_panes.language_panes_mgmt import LanguagePanesManager
from iwrap.settings.project import ProjectSettings
from iwrap.gui.settings.tooltip import ToolTip


class SettingsPane(ttk.Frame, IWrapPane):
    """Class displaying settings specific for a given language

    Attributes:
        documentation_editor (TextEditor): Allows to access the text editor object outside the class.

    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the settings pane tab.
        Args:
            master (ttk.Frame, optional): A parent widget.
        
        Note:
            Creates a template for a text editor from the TextEditor subclass
        """
        super().__init__(master)
        self.language_settings_pane : (IWrapPane, ttk.Frame) = None
        self.active_language = None


    def change_language(self, selected_language) -> None:
        """Update specific language pane when programming language in combobox is changed.

        Args:
            event: Combobox change value event object. Default to None.
        """
        if selected_language == self.active_language:
            return # no need to change the pane

        if self.language_settings_pane:
            self.language_settings_pane.pack_forget() # remove an old pane

        language_settings_pane_cls = LanguagePanesManager.get_language_pane( selected_language )
        if language_settings_pane_cls:
            self.language_settings_pane = language_settings_pane_cls(self)
            self.language_settings_pane.pack(fill=tk.BOTH, padx=5, pady=5)
            self.language_settings_pane.reload()

        self.active_language = selected_language


    def update_settings(self):
        """Update documentation in ProjectSettings.
        """
        if self.language_settings_pane:
                self.language_settings_pane.update_settings()

    def reload(self):
        """Immediately refresh the pane content.
        """

        selected_language = ProjectSettings.get_settings().code_description.implementation.programming_language
        self.change_language(selected_language)

        if self.language_settings_pane:
            self.language_settings_pane.reload()

