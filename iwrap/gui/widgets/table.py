import tkinter as tk
from tkinter import ttk

from iwrap.gui.widgets.scrollable_frame import ScrollableFrame


class Table( ttk.Frame ):
    """The Table class enables the creation of tables made of columns and rows. Rows can contain text cells and
     radio buttons. Table class has methods that enable select clicked row, move this row up and down in the table
    and delete the selected row. There is availability to add and edit row using the sheet.

    Attributes:
        frame (ScrollableFrame): The scrollable frame widget.
        rows ([[str]]): The list of list of strings contains data for rows.
        columns ([Columns]): The list of the Columns objects.
        selected_row (int, default=None): The number of the selected row.

    """
    def __init__(self, rows, columns, master=None, lost_focus_listeners=None):
        """Initialize the Table class object.

        Args:
            rows ([[str]]): The list of list of strings contains data for rows.
            columns ([Columns]): The list of the Columns objects.
            master (ttk.Frame): The master frame.
        """
        super().__init__(master)
        self.frame = ScrollableFrame(master)
        self.columns = []
        self.rows = []
        self.selected_row = None
        self.lost_focus_listeners = lost_focus_listeners

        self.add_new_table(rows, columns)

    def add_new_table(self, rows, columns):
        """Adds a new table to the frame.

        Args:
            rows ([[str]]): The list of list of strings contains data for rows.
            columns ([Columns]): The list of the Columns objects.
        """
        self.delete_data_from_table()
        self.columns = columns
        self._add_columns()
        self.add_rows(rows)

    def delete_data_from_table(self):
        """Delete all data from rows in the table.
        """
        for row in self.rows:
            for row_cell in row.row_cells:
                row_cell.cell.destroy()
                del row_cell
            del row
        self.rows = []
        self.selected_row = None

    def get_data_from_table(self):
        """Returns table with dictionaries contains data from all rows in the table.

        Returns (list): The table with dictionaries contains data from every row in the table.
        """
        table_data = []
        for row in self.rows:
            rows_values = row.get_row_values()
            table_data.append(rows_values)

        return table_data

    def add_rows(self, data):
        """Initializes the Row objects and adds them to table_row list.

        Args:
            data (list): The list of values needed to be added into cells in the row.
        """
        for row in data:
            row_number = len(self.rows) + 1
            table_row = Row(row_number, row, self.frame, self.columns)
            self.rows.append(table_row)
            for row_cell in table_row.row_cells:
                row_cell.cell.bind("<1>", lambda event, parent_row=table_row: self.select_row(parent_row))

        self.frame.update()

    def _add_columns(self):
        """Adds the Columns objects to the table grid.
        """
        for idx, column in enumerate(self.columns):
            self.frame.columnconfigure(idx, weight=1)
            column.add_column_to_grid(idx, self.frame)

    def select_row(self, parent_row):
        """Sets the number of selected row, change the color of selected row from white to light gray

        Args:
            parent_row (Row): The clicked Row object.
        """
        self.selected_row = parent_row.row_number
        for row in self.rows:
            for row_cell in row.row_cells:
                if row_cell.row_number == self.selected_row:
                    row_cell.change_color_to_lightgray()
                else:
                    row_cell.change_color_to_white()
        self.change_listeners_state()

    def change_listeners_state(self, *args):
        if self.lost_focus_listeners is not None:
            for listener in self.lost_focus_listeners:
                if self.selected_row is not None:
                    listener["state"] = "normal"
                else:
                    listener["state"] = "disabled"

    def delete_row(self):
        """Deletes the selected row from the table.
        """
        for row in self.rows:
            if row.row_number == self.selected_row:
                for row_cell in row.row_cells:
                    row_cell.cell.grid_forget()
                    row_cell.cell.destroy()
                    del row_cell
                self.rows.remove(row)
                del row
                break

        self._update_rows()

    def _update_rows(self):
        """Updates all rows position in the table grid, set selected_row to None
        """
        self.selected_row = None
        for row_number, row in enumerate(self.rows):
            for row_cell in row.row_cells:
                row_cell.row_number = row_number + 1
                row_cell.cell.grid(row=row_number + 1, column=row_cell.cell.grid_info()['column'])
            row.row_number = row_number + 1
        self.change_listeners_state()

    @staticmethod
    def _move_row_up(row_to_move):
        """Decreases row position in the table.

        Args:
            row_to_move (Row): The Row object in which row position will be decreased.
        """
        for row_cell in row_to_move.row_cells:
            row_cell.row_number = row_to_move.row_number - 1
            row_cell.cell.grid(row=row_to_move.row_number - 1, column=row_cell.column_number, sticky="ew")
        row_to_move.row_number = row_to_move.row_number - 1

    @staticmethod
    def _move_row_down(row_to_move):
        """Increases row position in the table.

        Args:
            row_to_move (Row): The Row object in which row position will be increased.
        """
        for row_cell in row_to_move.row_cells:
            row_cell.row_number = row_to_move.row_number + 1
            row_cell.cell.grid(row=row_to_move.row_number + 1, column=row_cell.column_number, sticky="ew")
        row_to_move.row_number = row_to_move.row_number + 1

    def row_up_feature(self):
        """Enables move row up in the table.
        """
        if self.selected_row not in [1, None]:
            top_row = [row for row in self.rows if row.row_number == self.selected_row - 1][0]
            current_row = [row for row in self.rows if row.row_number == self.selected_row][0]

            self._move_row_up(current_row)
            self.selected_row = self.selected_row - 1
            self._move_row_down(top_row)
            self.rows.sort(key=lambda x: x.row_number, reverse=False)

    def row_down_feature(self):
        """Enables move row down in the table.
        """
        if self.selected_row not in [len(self.rows), None]:
            bottom_row = [row for row in self.rows if row.row_number == self.selected_row + 1][0]
            current_row = [row for row in self.rows if row.row_number == self.selected_row][0]

            self._move_row_down(current_row)
            self.selected_row = self.selected_row + 1
            self._move_row_up(bottom_row)
            self.rows.sort(key=lambda x: x.row_number, reverse=False)

    def add_row(self):
        """Enables add a new row. The method creates an ArgumentWindow object what is associated with opening
         a new window with add row sheet.
        """
        new_window = ArgumentWindow(self)
        new_window.window.title("iWrap - Add new argument")
        tk.Button(new_window.footer, text='Add', command=new_window.add_new_row, width=8).pack(side=tk.RIGHT, padx=10)

    def edit_row(self):
        """Enables edit selected row. The method creates an ArgumentWindow object what is associated with opening
         a new window with a row editor.
        """
        selected_row_data = []
        if self.selected_row :
            for row in self.rows:
                if row.row_number == self.selected_row:
                    for idx, row_cell in enumerate(row.row_cells):
                        if isinstance(row_cell, RowRadioButton):
                            selected_row_data.append(row.selected_column_label.get())
                        elif isinstance(row_cell, RowEntry):
                            selected_row_data.append(row_cell.row_text.get())

            new_window = ArgumentWindow(self)
            new_window.window.title("iWrap - Edit argument")
            new_window.set_row_values(selected_row_data)
            tk.Button(new_window.footer, text='Close', command=new_window.edit_row, width=8).pack(side=tk.RIGHT, padx=10)


class ArgumentWindow:
    def __init__(self, master=None):
        self.window = tk.Toplevel(master)
        self.window.minsize(500, 100)
        self.window.geometry('500x200')
        self.window.focus_force()
        self.window.grab_set()

        content_frame = tk.Frame(self.window, height=300)
        self.footer = tk.Frame(self.window, bd=1, relief=tk.SUNKEN, height=50)
        self.footer.pack(side=tk.BOTTOM, fill=tk.X)
        content_frame.pack(side=tk.TOP, fill=tk.BOTH, expand=1)

        self.labelframe = ttk.LabelFrame(content_frame, text="Add new row")
        self.labelframe.pack(fill=tk.BOTH, expand=0, padx=3, pady=3)
        self.labelframe.columnconfigure(1, weight=1)
        self.master = master
        self.columns = self.master.columns
        self.new_cells = []
        self._add_content()

        tk.Button(self.footer, text='Cancel', command=self._close_add_window, width=8).pack(side=tk.RIGHT, padx=10)
        content_frame.update()

    def _add_content(self):
        """Add content to the ArgumentWindow.
        """
        radiobutton_combobox_cell_value = tk.StringVar()

        for idx, column in enumerate(self.columns):
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
                    combobox_values = [column.label_var.get() for column in self.columns
                                       if column.column_type == Column.RADIOBUTTON]
                    new_cell = ttk.Combobox(self.labelframe, state='readonly', values=combobox_values,
                                            textvariable=radiobutton_combobox_cell_value)
                    new_cell.current(0)
                    new_cell.grid(row=idx, column=1, sticky="ew", padx=10, pady=5)
                self.new_cells.append(radiobutton_combobox_cell_value)

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
            if row.row_number == self.master.selected_row:
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
    """The row class enables initialize text and radio button type cells in a specific row, set radio buttons values
     if any column has radio button column type, and get data from all cells in the row.

    Attributes:
        columns ([Column]): List of Column class objects.
        row_number (int): The row number to put cells in.
        row_cells (list): The list of cells related to row. Cell can be RowEntry or RowRadioButton object.
    """
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
        """Sets radio buttons values and makes them related to row.
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
    """The class RowRadioButton enables initialize radio button type table cells,
     add them to the grid and change color to white or light gray.


    Attributes:
        row_number (int): The row number where the cell is placed.
        column_number (int): The column number where the cell is placed.
        value (str): The cell data.
        cell (Entry): The cell Entry placed in the table grid.
    """
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
        self.cell = tk.Radiobutton(master, bg="white", state='disabled')
        self.cell.grid(row=row, column=column, sticky="ew")

    def change_color_to_lightgray(self):
        """Changes the Radiobutton color to lightgray.
        """
        self.cell.config(bg="lightgray")

    def change_color_to_white(self):
        """Changes the Radiobutton color to white.
        """
        self.cell.config(bg="white")

    def get_cell_value(self):
        """Returns the value of cell data.

        Returns (int): The value of cell data.
        """
        return self.value


class RowEntry:
    """The class RowEntry enables initialize text type table cells, add them to the grid, and change color
     to white or light gray.

    Attributes:
        row_number (int):  The row number where the cell is placed.
        column_number (int):  The column number where the cell is placed.
        row_text (StringVar): The cell data.
        cell (Entry): The cell Entry placed in the table grid.
    """
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
        self.cell = tk.Entry(master, text=self.row_text, state='readonly', readonlybackground="white", width=6)
        self.cell.grid(row=row, column=column, sticky="ew")

    def change_color_to_lightgray(self):
        """Changes the Entry color to light gray.
        """
        self.cell.config(readonlybackground="lightgray")

    def change_color_to_white(self):
        """Changes the Entry color to white.
        """
        self.cell.config(readonlybackground="white")

    def get_cell_value(self):
        """Returns value of cell data.

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
    TEXT = 'text'
    RADIOBUTTON = 'radiobutton'
    COMBOBOX = 'combobox'

    def __init__(self, column_type, table_label, data_label, list_of_values=[]):
        """Initialize the Column class object.

        Args:
            column_type (str): The column type. Column type should be selected from the Column class attributes
             (TEXT, RADIOBUTTON, COMBOBOX).
            table_label (str): The column label.
            data_label (str): The data label.
            list_of_values (str): The list of possible values in the column.
        """
        self.label_var = tk.StringVar()
        self.label_var.set(table_label)
        self.column_type = column_type
        self.list_of_values = list_of_values
        self.data_label = data_label

    def add_column_to_grid(self, position, master):
        """Adds a column to grid.

        Args:
            position (int): The column number to put Entry in.
            master (ttk.Frame): The master frame where Entry will be placed.
        """
        tk.Entry(master, textvariable=self.label_var, state='readonly', width=14, justify='center')\
            .grid(row=0, column=position, sticky="ew")
