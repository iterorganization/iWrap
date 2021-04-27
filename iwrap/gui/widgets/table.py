import tkinter as tk
from tkinter import ttk


class Table( ttk.Frame ):
    def __init__(self, data, columns, master=None):
        super().__init__(master)
        self.frame = ScrollableFrame(master)
        self.data = data
        self.columns = columns
        self.selected_row = None
        self._add_data()
        self.frame.update()

    def _add_data(self):
        for idx, column in enumerate(self.columns):
            self.frame.columnconfigure(idx, weight=1)
            Column(idx, column, self.frame)

        self.rows = []
        for row_number, row in enumerate(self.data):
            table_row = Row(row_number + 1, row, self.frame)
            self.rows.append(table_row)
            for row_cell in table_row.row_cells:
                row_cell.cell.bind("<1>", lambda event, parent_row=table_row: self._select_row(parent_row))

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
        pass


class Row:
    def __init__(self, row, data, master=None):
        self.row_number = row
        self.row_cells = []
        self.v = tk.BooleanVar()
        self.v.set(True)
        for idx, elem in enumerate(data):
            if isinstance(elem, str):
                self.row_cells.append(RowEntry(row, idx, elem, master))
            if isinstance(elem, bool):
                self.row_cells.append(RowRadioButton(row, idx, elem, master))

        self._make_radio_buttons_pairs()

    def _make_radio_buttons_pairs(self):
        self.v = tk.BooleanVar()
        self.v.set(True)
        for row_cell in self.row_cells:
            if isinstance(row_cell, RowRadioButton):
                row_cell.cell.config(variable=self.v, value=row_cell.value)


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
        row_text = tk.StringVar()
        row_text.set(text)
        self.cell = tk.Entry(master, textvariable=row_text, state='readonly', readonlybackground="white", width=6)
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
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y, expand=False)

        self.canvas = tk.Canvas(master, yscrollcommand=scrollbar.set)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        scrollbar.config(command=self.canvas.yview)
        self.canvas.bind('<Configure>', self._fill_canvas)

        ttk.Frame.__init__(self, master)
        self.windows_item = self.canvas.create_window(0, 0, window=self, anchor=tk.NW)

    def _fill_canvas(self, event):
        self.canvas.itemconfig(self.windows_item, width=event.width)

    def update(self):
        self.update_idletasks()
        self.canvas.config(scrollregion=self.canvas.bbox(self.windows_item))
