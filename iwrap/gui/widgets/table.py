import tkinter as tk
from tkinter import ttk


class Table( ttk.Frame ):
    def __init__(self, data, columns, master=None):
        self.selected_row = None

        # ADD COLUMNS
        for idx, column in enumerate(columns):
            Column(idx, column, master)

        # ADD ROWS
        self.rows = []
        for row_number, row in enumerate(data):
            table_row = Row(row_number + 1, row, master)
            self.rows.append(table_row)
            for entry in table_row.row_entries:
                entry.entry.bind("<1>", lambda event, row_num=table_row.row_number: self.select_row(row_num))

    def select_row(self, selected_row):
        self.selected_row = selected_row
        for row in self.rows:
            for entry in row.row_entries:
                if entry.row_number == selected_row:
                    entry.entry.config(readonlybackground="lightgray")
                else:
                    entry.entry.config(readonlybackground="white")

    def delete_row(self):
        for row in self.rows:
            if row.row_number == self.selected_row:
                for entry in row.row_entries:
                    entry.entry.destroy()
                    del entry
                self.rows.remove(row)
                del row

    def add_row(self):
        pass


class Row( ttk.Frame ):
    def __init__(self, row, data, master=None):
        self.row_number = row
        self.row_entries = []
        for idx, elem in enumerate(data):
            self.row_entries.append(RowEntry(row, idx, elem, master))


class RowEntry( ttk.Frame ):
    def __init__(self, row, column, text, master=None):
        self.row_number = row
        row_text = tk.StringVar()
        row_text.set(text)
        self.entry = tk.Entry(master, textvariable=row_text, state='readonly', readonlybackground="white", width=6)
        self.entry.grid(row=row, column=column, sticky="ew")


class Column( ttk.Frame ):
    def __init__(self, column, text, master=None):
        type_text = tk.StringVar()
        type_text.set(text)
        tk.Entry(master, textvariable=type_text, state='readonly', width=15, justify='center')\
            .grid(row=0, column=column, sticky="ew")
