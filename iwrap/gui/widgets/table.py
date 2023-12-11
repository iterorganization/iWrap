import logging
import tkinter as tk
from tkinter import ttk

from iwrap.gui.widgets.scrollable_frame import ScrollableFrame
from iwrap.gui.settings.tooltip import ToolTip


class Table( ttk.Frame ):
    """The Table class enables the creation of tables made of columns and rows. Rows can contain text cells and
    radio buttons. Table class has methods that enable select clicked row, move this row up and down in the table
    and delete the selected row. There is availability to add and edit row using the sheet.

    Attributes:
        frame (ScrollableFrame): The scrollable frame widget.
        rows ([[str]]): The list of list of strings contains data for rows.
        columns ([Columns]): The list of the Columns objects.
        selected_row (int, default=None): The number of the selected row.
        lost_focus_listeners(list): he list of buttons which state depends on whether the row is selected.

    ``Example``:

    .. code-block:: python

        columns = [Column(Column.TEXT, "Label1", "Name1"),
                Column(Column.RADIOBUTTON, "Input", "Name2"),
                Column(Column.RADIOBUTTON, "Output", "Name2"),
                Column(Column.TEXT, "Label4", "Name3")]
        values = [['example1', 'Input', 'Input', 'example2'],
                ['example3', 'Output', 'Output', 'example4']]
        Table(values, columns, master_frame)
    """


    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, rows, columns, master=None, lost_focus_listeners=None):

        """Initialize the Table class object.

        Args:
            rows ([[str]]): The list of list of strings contains data for rows.
            columns ([Columns]): The list of the Columns objects.
            master (ttk.Frame, None): The master frame.
            lost_focus_listeners (list, None): The list of buttons which state depends on whether the row is selected.
        """
        super().__init__(master)
        self.frame = ScrollableFrame(master)
        self.columns = []
        self.rows = []
        self.selected_row = tk.IntVar()
        self.selected_row.trace('w', self.change_listeners_state)
        self.lost_focus_listeners = lost_focus_listeners
        self._row_frames = []

        self.columns = columns
        self._add_columns()
        self.add_new_table_content(rows)

    def add_new_table_content(self, rows, row_to_select=0):
        """Add a new rows to the frame.

        Args:
            row_to_select (int): The number of the row that will be selected. Default to 0.
            rows ([[str]]): The list of list of strings contains data for rows.
        """
        self.delete_data_from_table()
        self.add_rows(rows)
        # select row
        if len(self.rows):
            self.select_row(self.rows[int(row_to_select)])
        else:
            self.selected_row.set(0)

    def delete_data_from_table(self):
        """Delete all data from rows in the table.
        """
        for row in self.rows:
            for row_cell in row.row_cells:
                row_cell.cell.pack_forget()
                row_cell.cell.destroy()
                del row_cell
            del row

        for row_frame in self._row_frames:
            row_frame.pack_forget()

        self.rows = []

    def get_data_from_table(self):
        """Return table with dictionaries contains data from all rows in the table.

        Returns (list): The table with dictionaries contains data from every row in the table.
        """
        table_data = []
        for row in self.rows:
            rows_values = row.get_row_values()
            table_data.append(rows_values)

        return table_data

    def add_rows(self, data):
        """Initialize the Row objects and adds them to table_row list.

        Args:
            data (list): The list of values needed to be added into cells in the row.
        """
        for row in data:
            row_frame = tk.Frame(self.frame)
            row_frame.pack(side="top", fill="x")
            self._row_frames.append(row_frame)
            row_number = len(self.rows) + 1
            table_row = Row(row_number, row, row_frame, self.columns)
            self.rows.append(table_row)
            for row_cell in table_row.row_cells:
                row_cell.cell.bind("<1>", lambda event, parent_row=table_row: self.select_row(parent_row))
                row_cell.cell.bind("<Double-Button-1>", lambda event, parent_row=table_row: self.show_info())

        self.frame.update()

    def show_info(self):
        """Show window with information about selected row.
        """
        for listener in self.lost_focus_listeners:
            if listener['text'] == "Info...":
                listener.invoke()

    def _add_columns(self):
        """Add the Columns objects to the table grid.
        """
        column_frame = tk.Frame(self.frame)
        column_frame.pack(side="top", fill="x")
        for idx, column in enumerate(self.columns):
            self.frame.columnconfigure(idx, weight=1)
            column.add_column_to_grid(column_frame)

    def get_selected_row(self):
        """Return number of selected row. If row is not selected method returns None.

        Returns (int): The row number or None if row is not selected.

        """
        if self.selected_row.get() == 0:
            return None
        return self.selected_row.get()

    def select_row(self, parent_row):
        """Set the number of selected row, change the color of selected row from white to light gray.

        Args:
            parent_row (Row): The clicked Row object.
        """
        self.selected_row.set(parent_row.row_number)
        for row in self.rows:
            for row_cell in row.row_cells:
                if row_cell.row_number == self.get_selected_row():
                    row_cell.change_color_to_lightgray()
                else:
                    row_cell.change_color_to_white()

    def change_listeners_state(self, *args):
        """Change the state of buttons from a list of buttons which state depends on whether the row is selected.
        """
        if self.lost_focus_listeners is not None:
            for listener in self.lost_focus_listeners:
                if self.get_selected_row() is not None:
                    listener["state"] = "normal"
                else:
                    listener["state"] = "disabled"

    def delete_row(self):
        """Delete the selected row from the table.
        """
        for row in self.rows:
            if row.row_number == self.get_selected_row():
                for row_cell in row.row_cells:
                    row_cell.cell.grid_forget()
                    row_cell.cell.destroy()
                    del row_cell
                self.rows.remove(row)
                del row
                break

        self._update_table(0)

    def _update_table(self, selected_row):
        """Collects data from all rows from the table grid, adds new table and sets selected_row value.

        Args:
            selected_row (int): The row number of selected row.
        """
        self.selected_row.set(selected_row)
        data = self.get_data_from_table()
        rows = []
        for row_data in data:
            cells_data = []
            for column in self.columns:
                cells_data.append(row_data[column.data_label])
            rows.append(cells_data)

        self.add_new_table_content(rows, self.get_selected_row() or 0)

    def row_up_feature(self):
        """Enable move row up in the table.
        """
        if self.get_selected_row() not in [1, None]:
            top_row = [row for row in self.rows if row.row_number == self.get_selected_row() - 1][0]
            current_row = [row for row in self.rows if row.row_number == self.get_selected_row()][0]
            self.rows[top_row.row_number - 1] = current_row
            self.rows[current_row.row_number - 1] = top_row
            self._update_table(top_row.row_number - 1)

    def row_down_feature(self):
        """Enable move row down in the table.
        """
        if self.get_selected_row() not in [len(self.rows), None]:
            bottom_row = [row for row in self.rows if row.row_number == self.get_selected_row() + 1][0]
            current_row = [row for row in self.rows if row.row_number == self.get_selected_row()][0]
            self.rows[bottom_row.row_number - 1] = current_row
            self.rows[current_row.row_number - 1] = bottom_row
            self._update_table(bottom_row.row_number - 1)

    def add_row(self, frame_title):
        """Enable add a new row. The method creates an ArgumentWindow object what is associated with opening
         a new window with add row sheet.

         Args:
             frame_title (str): The frame title.
        """
        new_window = ArgumentWindow(self)
        new_window.window.title(f"iWrap - Add new {frame_title}")
        new_window.labelframe['text'] = f"Add new {frame_title}"
        tk.Button(new_window.footer, text='Add', command=new_window.add_new_row, width=8).pack(side=tk.RIGHT, padx=10)

    def edit_row(self, frame_title):
        """Enable edit selected row. The method creates an ArgumentWindow object what is associated with opening a new
        window with a row editor.

        Args:
             frame_title (str): The frame title.
        """
        selected_row_data = []
        if self.get_selected_row():
            for row in self.rows:
                if row.row_number == self.get_selected_row():
                    for idx, row_cell in enumerate(row.row_cells):
                        if isinstance(row_cell, RowRadioButton):
                            selected_row_data.append(row.selected_column_label.get())
                        elif isinstance(row_cell, RowEntry):
                            selected_row_data.append(row_cell.row_text.get())

            new_window = ArgumentWindow(self)
            new_window.window.title(f"iWrap - Edit {frame_title}")
            new_window.labelframe['text'] = f"Edit {frame_title}"
            new_window.set_row_values(selected_row_data)
            tk.Button(new_window.footer, text='Close', command=new_window.edit_row, width=8).pack(side=tk.RIGHT, padx=10)

    def filter_table(self, filter_value, data):
        """Filter table by filter_value.

        Args:
            filter_value (str): The filter value.
            data([[str]]): The list of list of strings contains data for rows.
        """
        rows = []
        for row_data in data:
            if any(filter_value.upper() in cell_data.upper() for cell_data in row_data):
                rows.append(row_data)
        self.selected_row.set(0)
        self.add_new_table_content(rows)


class ArgumentWindow:
    """The ArgumentWindow class is dedicated to adding a new row or editing the selected row. Initializing the class
    object opens a new modal window where the column labels are in the first column and the column values are in the second
    column. On the bottom, there is a button that enables close window without saving.

    Attributes:
        window (tk.Toplevel): The new window for editing/adding row.
        footer (tk.Frame): The bottom side od the window.
        labelframe (ttk.LabelFrame): The main labelframe.
        master (Table): The master frame.
        new_cells (list): The list of column values from add/edit sheet.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, master=None):
        """Initialize the ArgumentWindow class object.

        Args:
            master (Table, None): The master frame.
        """
        self.window = tk.Toplevel(master)
        self.window.minsize(500, 100)
        self.window.geometry('500x200')
        self.window.resizable(width=False, height=True)
        self.window.focus_force()
        self.window.grab_set()

        content_frame = tk.Frame(self.window, height=300)
        self.footer = tk.Frame(self.window, bd=1, relief=tk.SUNKEN, height=50)
        self.footer.pack(side=tk.BOTTOM, fill=tk.X)
        content_frame.pack(side=tk.TOP, fill=tk.BOTH, expand=1)

        self.labelframe = ttk.LabelFrame(content_frame)
        self.labelframe.pack(fill=tk.BOTH, expand=0, padx=3, pady=3)
        self.labelframe.columnconfigure(1, weight=1)
        self.master = master
        self.new_cells = []
        self._add_content()

        tk.Button(self.footer, text='Cancel', command=self._close_add_window, width=8).pack(side=tk.RIGHT, padx=10)
        content_frame.update()

    def _add_content(self):
        """Add content to the ArgumentWindow. In the first column, there are labels which are columns name.
        The second column contains cells for column values. Depending on column type cells can be text, radio button,
         or combobox type.
        """
        radiobutton_combobox_cell_value = tk.StringVar()

        for idx, column in enumerate(self.master.columns):
            if column.column_type == Column.TEXT:
                self._set_label(idx, column.label_var.get())
                text_cell_value = tk.StringVar()
                new_cell = ttk.Entry(self.labelframe, textvariable=text_cell_value)
                new_cell.grid(row=idx, column=1, sticky="ew", padx=10, pady=5)
                self.new_cells.append(text_cell_value)

            elif column.column_type == Column.COMBOBOX:
                self._set_label(idx, column.label_var.get())
                combobox_cell_value = tk.StringVar()
                new_cell = ttk.Combobox(self.labelframe, state='readonly', values=column.list_of_values,
                                        textvariable=combobox_cell_value)
                new_cell.current(0)
                new_cell.grid(row=idx, column=1, sticky="ew", padx=10, pady=5)
                self.new_cells.append(combobox_cell_value)

            elif column.column_type == Column.RADIOBUTTON:
                if radiobutton_combobox_cell_value not in self.new_cells:
                    self._set_label(idx, column.data_label)
                    combobox_values = [column.label_var.get() for column in self.master.columns
                                       if column.column_type == Column.RADIOBUTTON]
                    new_cell = ttk.Combobox(self.labelframe, state='readonly', values=combobox_values,
                                            textvariable=radiobutton_combobox_cell_value)
                    new_cell.current(0)
                    new_cell.grid(row=idx, column=1, sticky="ew", padx=10, pady=5)
                self.new_cells.append(radiobutton_combobox_cell_value)
            ToolTip(new_cell, 'argument_window.' + column.label_var.get())

    def _set_label(self, row, label):
        """Set labels in the grid.

        Args:
            row (int): The row number to put Label in.
            label (string): The text to put to the Label.
        """
        ttk.Label(self.labelframe, text=f"{label}:").grid(row=row, column=0, sticky="ew", padx=10, pady=5)

    def set_row_values(self, data):
        """Set row values in the edit sheet.

        Args:
            data (list): List of string values from row.
        """
        for idx, cell in enumerate(self.new_cells):
            cell.set(data[idx])

    def add_new_row(self):
        """Add new row to the table.
        """
        new_row_data = []
        for idx, cell in enumerate(self.new_cells):
            new_row_data.append(cell.get())
        self.master.add_rows([new_row_data])
        self._close_add_window()

    def edit_row(self):
        """Edit selected row with values from the edit sheet.
        """
        for row in self.master.rows:
            if row.row_number == self.master.get_selected_row():
                for idx, row_cell in enumerate(row.row_cells):
                    if isinstance(row_cell, RowRadioButton):
                        row.selected_column_label.set(self.new_cells[idx].get())
                    elif isinstance(row_cell, RowEntry):
                        row_cell.row_text.set(self.new_cells[idx].get())
        self._close_add_window()

    def _close_add_window(self):
        """Close window.
        """
        self.window.destroy()


class Row:
    """The row class enables initialize text and radio button type cells in a specific row,
    set radio buttons values if any column has radio button column type, and get data from all cells in the row.

    Attributes:
        columns ([Column]): List of Column class objects.
        row_number (int): The row number to put cells in.
        row_cells (list): The list of cells related to row. A cell can be RowEntry or RowRadioButton object.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, row, data, master, columns):
        """Initialize the Row class object.

        Args:
            row (int): The row number to put cells in.
            data (list): The list of values needed to be an entry into cells.
            master (ttk.Frame): The master frame where cells will be placed.
            columns ([Column]): List of Column class objects.
        """
        self.columns = columns
        self.row_number = row
        self.row_cells = []
        for idx, elem in enumerate(data):
            if self.columns[idx].column_type == Column.COMBOBOX or self.columns[idx].column_type == Column.TEXT:
                self.row_cells.append(RowEntry(row, idx, elem, master))
            if self.columns[idx].column_type == Column.RADIOBUTTON:
                self.row_cells.append(RowRadioButton(row, idx, elem, master))

        if any(column.column_type == Column.RADIOBUTTON for column in self.columns):
            self._set_radiobuttons_values()

    def _set_radiobuttons_values(self):
        """Set radio buttons values and makes them related to row.
        """
        checked_column_label_id = [idx for idx, cell in enumerate(self.row_cells)
                                   if cell.get_cell_value() == self.columns[idx].label_var.get()][0]
        checked_column_label = self.columns[checked_column_label_id].label_var.get()

        self.selected_column_label = tk.StringVar()
        self.selected_column_label.set(checked_column_label)
        for idx, row_cell in enumerate(self.row_cells):
            if isinstance(row_cell, RowRadioButton):
                row_cell.cell.config(variable=self.selected_column_label, value=self.columns[idx].label_var.get())

    def get_row_values(self):
        """The method returns table row values.

        Returns (dict): Dictionary contains table row values. The key is column label and the value is cell data.
        """

        row_values = {}
        for idx, cell in enumerate(self.row_cells):
            column_data_label = self.columns[idx].data_label
            if isinstance(cell, RowRadioButton):
                row_values.update({column_data_label: self.selected_column_label.get()})
            elif isinstance(cell, RowEntry):
                row_values.update({column_data_label: cell.row_text.get()})

        return row_values


class RowRadioButton:
    """The class RowRadioButton enables initialize radio button type table cells, pack them to the master frame, and change
    color to white or light gray.

    Attributes:
        row_number (int): The row number where the cell is placed.
        column_number (int): The column number where the cell is placed.
        value (str): The cell data.
        cell (Entry): The cell Entry placed in the table grid.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, row, column, value, master):
        """Initialize the RowRadioButton class object.

        Args:
            row (int): The row number to put Entry in.
            column (int): The column number to put Entry in.
            value (str): The value of cell data.
            master (ttk.Frame): The master frame where Radiobutton will be placed.
        """
        self.row_number = row
        self.column_number = column
        self.value = value
        self.cell = tk.Radiobutton(master, bg="white", state=tk.DISABLED, width=1, highlightthickness=1, bd=0)
        self.cell.pack(side="left", fill="both", expand=True)

    def change_color_to_lightgray(self):
        """Change the Radiobutton color to lightgray.
        """
        self.cell.config(bg="lightgray")

    def change_color_to_white(self):
        """Change the Radiobutton color to white.
        """
        self.cell.config(bg="white")

    def get_cell_value(self):
        """Return the value of cell data.

        Returns (int): The value of cell data.
        """
        return self.value


class RowEntry:
    """The class RowEntry enables initialize text type table cells, pack them to the master frame and change color to white or light gray.

    Attributes:
        row_number (int):  The row number where the cell is placed.
        column_number (int):  The column number where the cell is placed.
        row_text (StringVar): The cell data.
        cell (Entry): The cell Entry placed in the table grid.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, row, column, text, master):
        """Initialize the RowEntry class object.

        Args:
            row (int): The row number to put Entry in.
            column (int): The column number to put Entry in.
            text (str): The cell data.
            master (ttk.Frame): The master frame where Entry will be placed.
        """
        self.row_number = row
        self.column_number = column
        self.row_text = tk.StringVar()
        self.row_text.set(text)
        self.cell = tk.Entry(master, text=self.row_text, state='readonly', readonlybackground="white", width=15,
                             relief=tk.FLAT, highlightthickness=1, justify='left')
        self.cell.pack(side="left", fill="both", expand=True)

    def change_color_to_lightgray(self):
        """Change the Entry color to light gray.
        """
        self.cell.config(readonlybackground="lightgray")

    def change_color_to_white(self):
        """Change the Entry color to white.
        """
        self.cell.config(readonlybackground="white")

    def get_cell_value(self):
        """Return value of cell data.

        Returns (int): The value of cell data.
        """
        return self.row_text.get()


class Column:
    """The column class enables define table columns, set their type, and add them to the grid.

    Attributes:
        TEXT (str): Defines text column type.
        RADIOBUTTON (str): Defines radiobutton column type.
        COMBOBOX (str): Defines combobox column type.
        label_var (StringVar): The column label.
        column_type (str): The column type.
        list_of_values (List[str, Any]): The list of possible values in the column. These values will be added to combobox
         in the Add/Edit window.
        data_label (str): The data label.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    TEXT = 'text'
    RADIOBUTTON = 'radiobutton'
    COMBOBOX = 'combobox'

    def __init__(self, column_type, table_label, data_label, list_of_values=None):
        """Initialize the Column class object.

        Args:
            column_type (str): The column type. Column type should be selected from the Column class attributes
             (TEXT, RADIOBUTTON, COMBOBOX).
            table_label (str): The column label.
            data_label (str): The data label.
            list_of_values (str, None): The list of possible values in the column.
        """
        if list_of_values is None:
            list_of_values = []
        self.label_var = tk.StringVar()
        self.label_var.set(table_label)
        self.column_type = column_type
        self.list_of_values = list_of_values
        self.data_label = data_label

    def add_column_to_grid(self, master):
        """Pack column to the master frame.

        Args:
            master (ttk.Frame): The master frame where Entry will be placed.
        """
        column_entry = tk.Entry(master, textvariable=self.label_var, state='readonly', width=13, justify='center')
        column_entry.pack(side="left", fill="both", expand=True)
        if self.column_type == Column.RADIOBUTTON:
            column_entry["width"] = 1
