import tkinter as tk
from tkinter import ttk


class Table( ttk.Frame ):
    def __init__(self, data, columns, master=None):
        super().__init__(master)
        self.frame = ScrollableFrame(master)
        self.columns_type = ['text', 'radiobutton', 'radiobutton', 'text']
        self.data = data
        self.columns = columns
        self.selected_row = None
        self.rows = []
        self._add_columns()
        self.add_data(self.data)

    def add_data(self, data):
        for row in data:
            row_number = len(self.rows) + 1
            table_row = Row(row_number, row, self.frame, self.columns)
            self.rows.append(table_row)
            for row_cell in table_row.row_cells:
                row_cell.cell.bind("<1>", lambda event, parent_row=table_row: self._select_row(parent_row))
        self.frame.update()

    def _add_columns(self):
        for idx, column in enumerate(self.columns):
            self.frame.columnconfigure(idx, weight=1)
            Column(idx, column, self.frame)

    def _select_row(self, parent_row):
        self.selected_row = parent_row.row_number
        for row in self.rows:
            for row_cell in row.row_cells:
                if row_cell.row_number == self.selected_row:
                    row_cell.change_color_to_lightgray()
                else:
                    row_cell.change_color_to_white()

    def delete_row(self):
        for row in self.rows:
            if row.row_number == self.selected_row:
                for row_cell in row.row_cells:
                    row_cell.cell.destroy()
                    del row_cell
                self.rows.remove(row)
                del row
        self._update_rows()

    def _update_rows(self):
        for row_number, row in enumerate(self.rows):
            for row_cell in row.row_cells:
                row_cell.row_number = row_number + 1
            row.row_number = row_number + 1
        self.selected_row = None

    @staticmethod
    def _move_row_up(row_to_move):
        for row_cell in row_to_move.row_cells:
            row_cell.row_number = row_to_move.row_number - 1
            row_cell.cell.grid(row=row_to_move.row_number - 1, column=row_cell.column_number, sticky="ew")
        row_to_move.row_number = row_to_move.row_number - 1

    @staticmethod
    def _move_row_down(row_to_move):
        for row_cell in row_to_move.row_cells:
            row_cell.row_number = row_to_move.row_number + 1
            row_cell.cell.grid(row=row_to_move.row_number + 1, column=row_cell.column_number, sticky="ew")
        row_to_move.row_number = row_to_move.row_number + 1

    def row_up_feature(self):
        if self.selected_row not in [1, None]:
            top_row = [row for row in self.rows if row.row_number == self.selected_row - 1][0]
            current_row = [row for row in self.rows if row.row_number == self.selected_row][0]

            self._move_row_up(current_row)
            self.selected_row = self.selected_row - 1
            self._move_row_down(top_row)
            self.rows.sort(key=lambda x: x.row_number, reverse=False)

    def row_down_feature(self):
        if self.selected_row not in [len(self.rows), None]:
            bottom_row = [row for row in self.rows if row.row_number == self.selected_row + 1][0]
            current_row = [row for row in self.rows if row.row_number == self.selected_row][0]

            self._move_row_down(current_row)
            self.selected_row = self.selected_row + 1
            self._move_row_up(bottom_row)
            self.rows.sort(key=lambda x: x.row_number, reverse=False)

    def add_row(self):
        RowDataWindow(self)

    def edit_row(self):
        selected_row_data = []
        if self.selected_row:
            for row in self.rows:
                if row.row_number == self.selected_row:
                    for idx, row_cell in enumerate(row.row_cells):
                        if isinstance(row_cell, RowRadioButton):
                            selected_row_data.append(row.selected_column_label.get())
                        elif isinstance(row_cell, RowEntry):
                            selected_row_data.append(row_cell.row_text.get())

            RowDataWindow(self, selected_row_data)


class RowDataWindow(Table):
    def __init__(self, master=None, data=None):
        self.window = tk.Toplevel(master)
        self.window.minsize(500, 100)
        self.window.geometry('500x200')

        content_frame = tk.Frame(self.window, height=300)
        footer = tk.Frame(self.window, bd=1, relief=tk.SUNKEN, height=50)
        footer.pack(side=tk.BOTTOM, fill=tk.X)
        content_frame.pack(side=tk.TOP, fill=tk.BOTH, expand=1)
        scrollable_frame = ScrollableFrame(content_frame)

        self.labelframe = ttk.LabelFrame(scrollable_frame, text="Add new row")
        self.labelframe.pack(fill=tk.BOTH, expand=0, padx=3, pady=3)
        self.labelframe.columnconfigure(1, weight=1)
        self.master = master
        self.columns = self.master.columns
        self.columns_type = self.master.columns_type
        self.data = data
        self.new_cells = []
        self._add_content()
        if self.data:
            self._set_row_values()

        tk.Button(footer, text='Close', command=self._close_add_window, width=8).pack(side=tk.RIGHT, padx=10)
        if not self.data:
            tk.Button(footer, text='Add', command=self._add_new_row, width=8).pack(side=tk.RIGHT, padx=10)
        else:
            tk.Button(footer, text='Edit', command=self._edit_row, width=8).pack(side=tk.RIGHT, padx=10)

        scrollable_frame.update()

    def _add_content(self):
        self.radiobutton_cell_value = tk.StringVar()
        for idx, column in enumerate(self.columns):
            column_type = self.columns_type[idx]
            column_label = tk.Label(self.labelframe, text=f"{column}:")
            column_label.grid(row=idx, column=0, sticky="ew", padx=10, pady=5)

            if column_type == 'text':
                text_cell_value = tk.StringVar()
                new_cell = tk.Entry(self.labelframe, textvariable=text_cell_value)
                new_cell.grid(row=idx, column=1, sticky="ew", padx=10, pady=5)
                self.new_cells.append(text_cell_value)

            elif column_type == 'radiobutton':
                self.radiobutton_cell_value.set(" ")
                new_cell = tk.Radiobutton(self.labelframe, variable=self.radiobutton_cell_value, value=column, bg="white")
                new_cell.grid(row=idx, column=1, sticky="ew", padx=10, pady=5)
                self.new_cells.append(self.radiobutton_cell_value)

    def _set_row_values(self):
        for idx, cell in enumerate(self.new_cells):
            cell.set(self.data[idx])

    def _add_new_row(self):
        new_row_data = []
        for idx, cell in enumerate(self.new_cells):
            if self.columns_type[idx] == 'radiobutton':
                new_row_data.append(cell.get() == self.columns[idx])
            else:
                new_row_data.append(cell.get())
        self.master.add_data([new_row_data])
        self._close_add_window()

    def _edit_row(self):
        for row in self.master.rows:
            if row.row_number == self.master.selected_row:
                for idx, row_cell in enumerate(row.row_cells):
                    if isinstance(row_cell, RowRadioButton):
                        row.selected_column_label.set(self.new_cells[idx].get())
                    elif isinstance(row_cell, RowEntry):
                        row_cell.row_text.set(self.new_cells[idx].get())
        self._close_add_window()

    def _close_add_window(self):
        self.window.destroy()


class Row:
    def __init__(self, row, data, master=None, columns_labels=None):
        self.columns_labels = columns_labels
        self.row_number = row
        self.row_cells = []
        self.v = tk.BooleanVar()
        self.v.set(True)
        for idx, elem in enumerate(data):
            if isinstance(elem, str):
                self.row_cells.append(RowEntry(row, idx, elem, master))
            if isinstance(elem, bool):
                self.row_cells.append(RowRadioButton(row, idx, elem, master))

        self._make_radio_buttons_pairs() #nie pary bo moga byc 3

    def _make_radio_buttons_pairs(self):
        checked_column_label_id = [idx for idx, cell in enumerate(self.row_cells) if cell.value is True][0]
        checked_column_label = self.columns_labels[checked_column_label_id]
        self.selected_column_label = tk.StringVar()
        self.selected_column_label.set(checked_column_label)
        for idx, row_cell in enumerate(self.row_cells):
            if isinstance(row_cell, RowRadioButton):
                row_cell.cell.config(variable=self.selected_column_label, value=self.columns_labels[idx])


class RowRadioButton:
    def __init__(self, row, column, value, master=None):
        self.row_number = row
        self.column_number = column
        self.value = value
        self.cell = tk.Radiobutton(master, bg="white")
        self.cell.grid(row=row, column=column, sticky="ew")

    def change_color_to_lightgray(self):
        self.cell.config(bg="lightgray")

    def change_color_to_white(self):
        self.cell.config(bg="white")


class RowEntry:
    def __init__(self, row, column, text, master=None):
        self.row_number = row
        self.column_number = column
        self.value = text
        self.row_text = tk.StringVar()
        self.row_text.set(text)
        self.cell = tk.Entry(master, text=self.row_text, state='readonly', readonlybackground="white", width=6)
        self.cell.grid(row=row, column=column, sticky="ew")

    def change_color_to_lightgray(self):
        self.cell.config(readonlybackground="lightgray")

    def change_color_to_white(self):
        self.cell.config(readonlybackground="white")


class Column:
    def __init__(self, column, text, master=None):
        type_text = tk.StringVar()
        type_text.set(text)
        tk.Entry(master, textvariable=type_text, state='readonly', width=14, justify='center')\
            .grid(row=0, column=column, sticky="ew")


class ScrollableFrame( ttk.Frame ):
    def __init__(self, master):
        scrollbar = tk.Scrollbar(master)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, expand=0)

        self.canvas = tk.Canvas(master, yscrollcommand=scrollbar.set)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=1)
        scrollbar.config(command=self.canvas.yview)
        self.canvas.bind('<Configure>', self._fill_canvas)

        ttk.Frame.__init__(self, master)
        self.windows_item = self.canvas.create_window(0, 0, window=self, anchor=tk.NW)

    def _fill_canvas(self, event):
        self.canvas.itemconfig(self.windows_item, width=event.width)

    def update(self):
        self.update_idletasks()
        self.canvas.config(scrollregion=self.canvas.bbox(self.windows_item))
