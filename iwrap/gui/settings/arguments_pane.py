import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

from iwrap.generation_engine.engine import Engine
from iwrap.gui.generics import IWrapPane
from iwrap.gui.widgets.table import Table
from iwrap.gui.widgets.table import Column
from iwrap.settings.project import ProjectSettings


class ArgumentsPane( ttk.Frame, IWrapPane ):
    """The ArgumentsPane contains a combobox with values from available data types and a table with arguments. The buttons
    on the right side of the table allow to add a new row, modify the selected row, move the row up or down, and delete
    the selected row. The table is automatically filled with the argument values from the ProjectSettings.

    Attributes:
        arguments_settings ([dict]): The list of arguments from the ProjectSettings.
        columns ([Column]): The list of the Column class objects.
        data_type (string): The data type value from the ProjectSettings.
        table (Table): The table widget.
        data_type_combobox (ttk.Combobox): The data type combobox.
    """
    def __init__(self, master=None):
        """Initialize the ArgumentsPane class object.

        Args:
            master (ttk.Frame, None): The master frame.
        """
        super().__init__( master )
        self.arguments_settings = None
        self.data_type = None

        # LABEL FRAME
        labelframe = ttk.LabelFrame(self, text="Arguments", borderwidth=2, relief="groove")
        labelframe.pack(fill=tk.BOTH, pady=10, expand=1)

        # COMBOBOX FRAME
        combobox_frame = ttk.Frame(labelframe)
        combobox_frame.pack(fill=tk.X, side=tk.TOP, pady=5)

        # COMBOBOX
        ttk.Label(combobox_frame, text="Data type:").pack(fill=tk.X, side=tk.LEFT, padx=10)
        self.data_type_combobox = ttk.Combobox(combobox_frame, state='readonly')
        self.data_type_combobox['values'] = Engine().active_generator.code_data_types
        self.data_type_combobox.current(0)
        self.data_type_combobox.pack(fill=tk.X, side=tk.RIGHT, expand=1, padx=10)

        # MAIN CONTENT FRAME
        main_content_frame = ttk.Frame(labelframe)
        main_content_frame.pack(fill=tk.BOTH, expand=1, padx=3, pady=3)

        # TABLE FRAME
        table_frame = tk.Frame(main_content_frame, highlightbackground="black", highlightthickness=1)
        table_frame.pack(fill=tk.BOTH, side=tk.LEFT, expand=1, padx=3, pady=3)

        # BUTTONS FRAME
        buttons_frame = ttk.Frame(main_content_frame, width=100)
        buttons_frame.pack(fill=tk.BOTH, side=tk.RIGHT, expand=0, padx=3, pady=3)
        buttons_frame_center = ttk.Frame(buttons_frame)
        buttons_frame_center.place(in_=buttons_frame, anchor="center", relx=.5, rely=.5)

        # TABLE
        # Todo: List of IDSes should be loaded dynamically from IMAS
        IDS = ["bremsstrahlung_visible", "calorimetry", "camera_ir", "camera_visible", "charge_exchange",
                "coils_non_axisymmetric", "controllers", "core_instant_changes", "core_profiles", "core_sources",
                "core_transport", "cryostat", "dataset_description", "dataset_fair", "disruption",
                "distribution_sources", "distributions", "divertors", "ec_launchers", "ece", "edge_profiles",
                "edge_sources", "edge_transport", "em_coupling", "equilibrium", "gas_injection", "gas_pumping",
                "gyrokinetics", "hard_x_rays", "ic_antennas", "interferometer", "iron_core", "langmuir_probes",
                "lh_antennas", "magnetics", "mhd", "mhd_linear", "mse", "nbi", "neutron_diagnostic", "ntms",
                "numerics", "pellets", "pf_active", "pf_passive", "polarimeter", "pulse_schedule", "radiation",
                "reflectometer_profile", "refractometer", "sawteeth", "sdn", "soft_x_rays", "spectrometer_mass",
                "spectrometer_uv", "spectrometer_visible", "spectrometer_x_ray_crystal", "summary", "temporary",
                "thomson_scattering", "tf", "transport_solver_numerics", "turbulence", "wall", "waves"]

        self.columns = [Column(Column.TEXT, "Label", "Name"),
                        Column(Column.RADIOBUTTON, "Input", "Intent"),
                        Column(Column.RADIOBUTTON, "Output", "Intent"),
                        Column(Column.COMBOBOX, "Type", "Type", IDS)]

        # BUTTONS
        buttons = []
        add_button = ttk.Button(buttons_frame_center, text="Add...", width=10)
        add_button.pack(side=tk.TOP, expand=1)

        edit_button = ttk.Button(buttons_frame_center, text="Edit...", state='disabled', width=10)
        edit_button.pack(side=tk.TOP, expand=1, pady=10)
        buttons.append(edit_button)

        up_button = ttk.Button(buttons_frame_center, text="Up", state='disabled', width=10)
        up_button.pack(side=tk.TOP, expand=1)
        buttons.append(up_button)

        down_button = ttk.Button(buttons_frame_center, text="Down", state='disabled', width=10)
        down_button.pack(side=tk.TOP, expand=1)
        buttons.append(down_button)

        remove_button = ttk.Button(buttons_frame_center, text="Remove", state='disabled', width=10)
        remove_button.pack(side=tk.TOP, expand=1, pady=10)
        buttons.append(remove_button)

        # TABLE
        self.table = Table([], self.columns, table_frame, buttons)

        # BIND BUTTONS WITH TABLE METHODS
        add_button['command'] = lambda: self.table.add_row("argument")
        edit_button['command'] = lambda: self.table.edit_row("argument")
        up_button['command'] = self.table.row_up_feature
        down_button['command'] = self.table.row_down_feature
        remove_button['command'] = self.table.delete_row

    def reload(self):
        """Reload arguments settings and data type from the ProjectSettings, add arguments to the table. If data type
        from the ProjectSettings is not available in combobox warning message box will be shown and the default value
        of data type will be selected in combobox.
        """
        self.data_type_combobox['values'] = Engine().active_generator.code_data_types
        self.arguments_settings = ProjectSettings.get_settings().code_description.arguments
        self.data_type = ProjectSettings.get_settings().code_description.data_type
        self.set_data_to_table()

        if self.data_type not in self.data_type_combobox['values']:
            self.data_type_combobox.current(0)
        elif self.data_type not in self.data_type_combobox['values'] and self.data_type is not None:
            messagebox.showwarning("Warning", f"Unknown data type. "
                                              f"The data type set to "
                                              f"{self.data_type_combobox.get()}.")
        else:
            self.data_type_combobox.set(self.data_type)

    def update_settings(self):
        """Update arguments and data type in the ProjectSettings.
        """
        arguments = self.get_data_from_table()
        data_type = self.data_type_combobox.get()
        ProjectSettings.get_settings().code_description.arguments = arguments
        ProjectSettings.get_settings().code_description.data_type = data_type

    def set_data_to_table(self):
        """Set data from arguments settings to the table.
        """
        table_data = []
        intent = {"IN": "Input", "OUT": "Output"}
        for argument in self.arguments_settings:
            table_data.append([argument['name'], intent[argument['intent']],
                               intent[argument['intent']], argument['type']])

        self.table.add_new_table_content(table_data)

    def get_data_from_table(self):
        """Return data from table.

        Returns: The list of data from each row from the table.
        """
        table_data = self.table.get_data_from_table()
        intent = {"Input": "IN", "Output": "OUT"}
        arguments = []
        for row in table_data:
            name = row['Name']
            row_type = row['Type']
            intent_value = intent[row['Intent']]
            arguments.append({'name': name, 'type': row_type, 'intent': intent_value})

        return arguments
