import tkinter as tk
from tkinter import ttk


class Table( ttk.Frame ):
    def __init__(self, data, columns, master=None):

        # ADD COLUMNS
        for idx, column in enumerate(columns):
            Column(idx, column, master)

        # ADD ROWS
        row_number = 1
        self.rows = []

        for row in data:
            for idx, elem in enumerate(row):
                row_entry = RowEntry(row_number, idx, elem, master)
                row_entry.entry.bind("<1>", lambda event, row_num=row_entry.row_number: self.select_row(row_num))
                self.rows.append(row_entry)
            row_number += 1

    def select_row(self, selected_row):
        for row in self.rows:
            if row.row_number == selected_row:
                row.entry.config(bg="lightgray")
            else:
                row.entry.config(bg="white")

    def delete_row(self):
        pass

    def add_row(self):
        pass


class RowEntry( ttk.Frame ):
    def __init__(self, row, column, text, master=None):
        self.row_number = row
        self.entry = tk.Entry(master, bg="white", width=6)
        self.entry.insert(0, text)
        self.entry.grid(row=row, column=column, sticky="ew")


class Column( ttk.Frame ):
    def __init__(self, column, text, master=None):
        type_text = tk.StringVar()
        type_text.set(text)
        tk.Entry(master, textvariable=type_text, state='readonly', width=15, justify='center')\
            .grid(row=0, column=column, sticky="ew")
